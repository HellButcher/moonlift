use std::{
    io::{self, Read},
    ops::Deref,
    pin::Pin,
};

use crate::{
    codegen_state::{Constant, Proto},
    opcode::{Op, OpCode},
    Bytecode,
};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum DeserialisationError {
    #[error("Invalid opcode value {0:?}")]
    InvalidOpCode(u8),
}

fn write_dynamic_u64(out: &mut impl io::Write, mut n: u64) -> Result<(), io::Error> {
    while n > 0x7F {
        out.write_all(&[((n & 0x7F) as u8) | 0x80])?;
        n >>= 7;
    }
    out.write_all(&[(n & 0x7F) as u8])?;
    Ok(())
}

fn read_dynamic_u64(read: &mut impl io::Read) -> Result<u64, io::Error> {
    let mut result = 0;
    let mut shift = 0;
    loop {
        let mut buf = [0u8; 1];
        read.read_exact(&mut buf)?;
        let byte = buf[0];
        result |= ((byte & 0x7F) as u64) << shift;
        if (byte & 0x80) == 0 {
            break;
        }
        shift += 7;
    }
    Ok(result)
}

impl Bytecode {
    pub const VERSION: u32 = 1;
    pub const MAGIC: [u8; 8] = *b"MOONLIFT";

    pub fn write_binary(&self, out: &mut impl io::Write) -> Result<(), io::Error> {
        out.write_all(&Self::MAGIC)?;
        out.write_all(&Self::VERSION.to_le_bytes())?;
        self.root.write_binary(out)
    }

    pub fn read_binary(read: &mut impl io::Read) -> Result<Self, io::Error> {
        let mut magic = [0u8; 8];
        read.read_exact(&mut magic)?;
        if magic != Self::MAGIC {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid magic number",
            ));
        }
        let mut version_buf = [0u8; 4];
        read.read_exact(&mut version_buf)?;
        let version = u32::from_le_bytes(version_buf);
        if version != Self::VERSION {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Unsupported version",
            ));
        }
        let root = Proto::read_binary(read)?;
        Ok(Self { root })
    }
}

impl Proto {
    pub fn write_binary(&self, out: &mut impl io::Write) -> Result<(), io::Error> {
        out.write_all(&(self.num_params as u16).to_le_bytes())?;
        out.write_all(&(self.is_vararg as u16).to_le_bytes())?;
        out.write_all(&(self.max_stack_size as u16).to_le_bytes())?;
        out.write_all(&(self.constants.len() as u16).to_le_bytes())?;
        out.write_all(&(self.num_upvalues as u16).to_le_bytes())?;
        out.write_all(&(self.protos.len() as u16).to_le_bytes())?;
        out.write_all(&(self.bytecode.len() as u32).to_le_bytes())?;
        out.write_all(&[0u8; 4])?; // reserved
                                   // write constants
        for constant in &self.constants {
            constant.write_binary(out)?;
        }
        // write code
        for op in &self.bytecode {
            out.write_all(&op.to_le_bytes())?;
        }
        // write nested protos
        for proto in &self.protos {
            proto.write_binary(out)?;
        }
        Ok(())
    }

    pub fn read_binary(read: &mut impl io::Read) -> Result<Self, io::Error> {
        let mut buf2 = [0u8; 2];
        let mut buf4 = [0u8; 4];

        read.read_exact(&mut buf2)?;
        let num_params = u16::from_le_bytes(buf2) as u8;
        read.read_exact(&mut buf2)?;
        let is_vararg = u16::from_le_bytes(buf2) != 0;
        read.read_exact(&mut buf2)?;
        let max_stack_size = u16::from_le_bytes(buf2) as u8;
        read.read_exact(&mut buf2)?;
        let num_constants = u16::from_le_bytes(buf2) as usize;
        read.read_exact(&mut buf2)?;
        let num_upvalues = u16::from_le_bytes(buf2) as u8;
        read.read_exact(&mut buf2)?;
        let num_protos = u16::from_le_bytes(buf2) as usize;
        read.read_exact(&mut buf4)?;
        let bytecode_len = u32::from_le_bytes(buf4) as usize;
        read.read_exact(&mut buf4)?; // reserved, ignore

        // read constants
        let mut constants = Vec::with_capacity(num_constants);
        for _ in 0..num_constants {
            constants.push(Constant::read_binary(read)?);
        }

        // read code
        let mut bytecode = Vec::with_capacity(bytecode_len);
        for _ in 0..bytecode_len {
            let mut op_buf = [0u8; 4];
            read.read_exact(&mut op_buf)?;
            let op = Op::from_le_bytes(op_buf)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            bytecode.push(op);
        }

        // read nested protos
        let mut protos = Vec::with_capacity(num_protos);
        for _ in 0..num_protos {
            protos.push(Proto::read_binary(read)?);
        }

        Ok(Proto {
            num_params,
            is_vararg,
            num_upvalues,
            max_stack_size,
            bytecode: bytecode.into_boxed_slice(),
            constants: constants.into_boxed_slice(),
            protos: protos.into_boxed_slice(),
        })
    }
}

impl Constant {
    pub fn write_binary(&self, out: &mut impl io::Write) -> Result<(), io::Error> {
        match self {
            Constant::Integer(i) => {
                if *i < 0 {
                    out.write_all(&[1u8])?;
                    write_dynamic_u64(out, (-*i) as u64)?;
                } else {
                    out.write_all(&[2u8])?;
                    write_dynamic_u64(out, *i as u64)?;
                }
            }
            Constant::Float(f) => {
                out.write_all(&[3u8])?;
                out.write_all(&(*f).to_le_bytes())?;
            }
            Constant::String(s) => {
                write_dynamic_u64(out, s.len() as u64 + 8)?;
                let bytes: &[u8] = s.deref();
                out.write_all(bytes)?;
            }
        }
        Ok(())
    }

    pub fn read_binary(read: &mut impl io::Read) -> Result<Self, io::Error> {
        let mut type_buf = [0u8; 1];
        read.read_exact(&mut type_buf)?;
        match type_buf[0] {
            1 => {
                let n = read_dynamic_u64(read)?;
                Ok(Constant::Integer(-(n as i64)))
            }
            2 => {
                let n = read_dynamic_u64(read)?;
                Ok(Constant::Integer(n as i64))
            }
            3 => {
                let mut float_buf = [0u8; 8];
                read.read_exact(&mut float_buf)?;
                Ok(Constant::Float(f64::from_le_bytes(float_buf)))
            }
            n if n >= 8 => {
                let str_len = if n >= 0x80 {
                    ((read_dynamic_u64(read)? << 7 | ((n as u64) & 0x7F)) - 8) as usize
                } else {
                    (n as usize) - 8
                };
                let mut buf = Vec::with_capacity(str_len);
                read.take(str_len as u64).read_to_end(&mut buf)?;
                Ok(Constant::String(Pin::from(buf.into_boxed_slice())))
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Unknown constant type",
            )),
        }
    }
}

impl Op {
    #[inline]
    pub const fn from_le_bytes(bytes: [u8; 4]) -> Result<Self, DeserialisationError> {
        let op = bytes[0];
        if op < OpCode::MIN || op > OpCode::MAX {
            return Err(DeserialisationError::InvalidOpCode(op));
        }
        Ok(unsafe { std::mem::transmute::<[u8; 4], Op>(bytes) })
    }

    #[inline]
    pub const fn to_le_bytes(self) -> [u8; 4] {
        unsafe { std::mem::transmute::<Op, [u8; 4]>(self) }
    }
}
