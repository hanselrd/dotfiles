use crate::*;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub enum Command {
    CreateDirectories {
        paths: Vec<PathBuf>,
    },
    CopyFiles {
        sources: Vec<PathBuf>,
        destination: PathBuf,
        mode: Option<dfa::types::FileMode>,
    },
    UnarchiveFiles {
        sources: Vec<PathBuf>,
        destination: PathBuf,
        mode: Option<dfa::types::FileMode>,
    },
    ExecuteCommands {
        commands: Vec<String>,
        executable: Option<String>,
    },
}

impl From<Command> for Vec<dfa::Task> {
    fn from(command: Command) -> Self {
        match command {
            Command::CreateDirectories { paths } => vec![dfa::Task {
                name: Some(String::from("Create directory (or directories)")),
                file: match globals::CONFIG.system {
                    dft::System::MacOS | dft::System::Linux => Some(dfa::modules::File {
                        path: PathBuf::from("{{ item }}"),
                        state: Some(dfa::modules::file::State::Directory),
                        ..Default::default()
                    }),
                    _ => None,
                },
                win_file: match globals::CONFIG.system {
                    dft::System::Windows => Some(dfa::modules::WinFile {
                        path: PathBuf::from("{{ item }}"),
                        state: Some(dfa::modules::win_file::State::Directory),
                        ..Default::default()
                    }),
                    _ => None,
                },
                r#loop: Some(String::from("{{ directories }}")),
                vars: Some(HashMap::from([(
                    String::from("directories"),
                    serde_yaml::Value::Sequence(
                        paths
                            .iter()
                            .map(|p| serde_yaml::Value::String(String::from(p.to_string_lossy())))
                            .collect(),
                    ),
                )])),
                ..Default::default()
            }],
            Command::CopyFiles {
                sources,
                destination,
                mode,
            } => vec![dfa::Task {
                name: Some(String::from("Copy file(s)")),
                copy: match globals::CONFIG.system {
                    dft::System::MacOS | dft::System::Linux => Some(dfa::modules::Copy {
                        src: Some(PathBuf::from("{{ item }}")),
                        dest: destination.join("{{ item }}"),
                        mode: mode,
                        force: Some(true),
                        ..Default::default()
                    }),
                    _ => None,
                },
                win_copy: match globals::CONFIG.system {
                    dft::System::Windows => Some(dfa::modules::WinCopy {
                        src: Some(PathBuf::from("{{ item }}")),
                        dest: destination.join("{{ item }}"),
                        force: Some(true),
                        ..Default::default()
                    }),
                    _ => None,
                },
                r#loop: Some(String::from("{{ files }}")),
                vars: Some(HashMap::from([(
                    String::from("files"),
                    serde_yaml::Value::Sequence(
                        sources
                            .iter()
                            .map(|s| serde_yaml::Value::String(String::from(s.to_string_lossy())))
                            .collect(),
                    ),
                )])),
                ..Default::default()
            }],
            Command::UnarchiveFiles {
                sources,
                destination,
                mode,
            } => todo!(),
            Command::ExecuteCommands {
                commands,
                executable,
            } => todo!(),
        }
    }
}
