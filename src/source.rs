use std::{convert::Infallible, io};


pub trait Source {
    type Error: std::error::Error;
    fn read_next(&mut self) -> Result<Option<u8>, Self::Error>;
    fn unwind(&mut self);
    fn pos(&self) -> usize;
}

const BUF_SIZE: usize = 512; // needs to be a power of two!
const POS_EOF: usize = !0;

pub struct ReadSource<R> {
    read: R,
    pos: usize,
    buf_pos: usize,
    buf_end: usize,
    buf: [u8; BUF_SIZE],
}

impl<R> ReadSource<R> {
    #[inline]
    pub const fn new(read: R) -> Self {
        Self {
            read,
            pos: 0,
            buf_pos: 0,
            buf_end: 0,
            buf: [0; BUF_SIZE],
        }
    }
    #[inline]
    pub fn into_inner(self) -> R {
        self.read
    }
}

impl<R: io::Read> Source for ReadSource<R> {
    type Error = io::Error;

    #[inline]
    fn read_next(&mut self) -> io::Result<Option<u8>> {
        if self.pos == POS_EOF {
            return Ok(None);
        }
        if self.buf_pos >= self.buf_end {
            // re-fill buffer
            self.buf_end = self.read.read(&mut self.buf)?;
            self.buf_pos = 0;
            if self.buf_end == 0 {
                self.pos = POS_EOF;
                return Ok(None);
            }
        }

        // SAFETY: bounds checked
        let c = unsafe { *self.buf.get_unchecked(self.buf_pos) };
        self.pos += 1;
        self.buf_pos += 1;
        Ok(Some(c))
    }

    #[inline]
    fn unwind(&mut self) {
        if self.pos != POS_EOF {
            debug_assert_ne!(self.buf_pos, 0);
            self.pos -= 1;
            self.buf_pos -= 1;
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }
}

pub struct BytesSource<T> {
    data: T,
    pos: usize,
}

impl<T: AsRef<[u8]>> BytesSource<T> {
    #[inline]
    pub const fn new(data: T) -> Self {
        Self { data, pos: 0 }
    }
    #[inline]
    pub fn into_inner(self) -> T {
        self.data
    }
}

impl<T: AsRef<[u8]>> Source for BytesSource<T> {
    type Error = Infallible;

    #[inline]
    fn read_next(&mut self) -> Result<Option<u8>, Self::Error> {
        let data = self.data.as_ref();
        let len = data.len();
        if self.pos < len {
            // SAFETY: bounds checked
            let c = unsafe { *data.get_unchecked(self.pos) };
            self.pos += 1;
            Ok(Some(c))
        } else {
            self.pos = POS_EOF;
            Ok(None)
        }
    }
    #[inline]
    fn unwind(&mut self) {
        if self.pos != POS_EOF {
            debug_assert_ne!(self.pos, 0);
            self.pos -= 1;
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        self.pos
    }
}
