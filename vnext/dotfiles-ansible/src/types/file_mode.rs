use bitflags::bitflags;
use serde::{Serialize, Serializer};
use std::fmt;

bitflags! {
    #[derive(Default)]
    pub struct FileModeBits : u8 {
        const READ = 0b00000001;
        const WRITE = 0b00000010;
        const EXECUTE = 0b00000100;
    }
}

impl fmt::Display for FileModeBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.contains(Self::READ) {
            write!(f, "r")?;
        }
        if self.contains(Self::WRITE) {
            write!(f, "w")?;
        }
        if self.contains(Self::EXECUTE) {
            write!(f, "x")?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileMode {
    pub user: FileModeBits,
    pub group: FileModeBits,
    pub other: FileModeBits,
}

impl fmt::Display for FileMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "u={},g={},o={}", self.user, self.group, self.other)
    }
}

impl Serialize for FileMode {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_mode_bits() {
        assert_eq!(FileModeBits::empty().to_string(), "");
        assert_eq!(FileModeBits::READ.to_string(), "r");
        assert_eq!(FileModeBits::WRITE.to_string(), "w");
        assert_eq!(FileModeBits::EXECUTE.to_string(), "x");
        assert_eq!((FileModeBits::READ | FileModeBits::WRITE).to_string(), "rw");
        assert_eq!((FileModeBits::WRITE | FileModeBits::READ).to_string(), "rw");
        assert_eq!(
            (FileModeBits::WRITE | FileModeBits::EXECUTE).to_string(),
            "wx"
        );
        assert_eq!(
            (FileModeBits::EXECUTE | FileModeBits::WRITE).to_string(),
            "wx"
        );
        assert_eq!(FileModeBits::all().to_string(), "rwx");
    }

    #[test]
    fn file_mode() {
        assert_eq!(
            FileMode {
                ..Default::default()
            }
            .to_string(),
            "u=,g=,o="
        );
        assert_eq!(
            FileMode {
                user: FileModeBits::READ,
                group: FileModeBits::WRITE,
                other: FileModeBits::EXECUTE
            }
            .to_string(),
            "u=r,g=w,o=x"
        );
        assert_eq!(
            FileMode {
                user: FileModeBits::all(),
                group: FileModeBits::all(),
                other: FileModeBits::all()
            }
            .to_string(),
            "u=rwx,g=rwx,o=rwx"
        );
    }
}
