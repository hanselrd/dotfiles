use crate::modules::*;
use crate::types::*;
use serde::Serialize;
use serde_yaml;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct Task {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#become: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub become_user: Option<String>,

    #[serde(rename = "ansible.builtin.copy")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub copy: Option<copy::Copy>,

    #[rustfmt::skip]
    // #[serde(rename = "ansible.windows.win_copy")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_copy: Option<win_copy::WinCopy>,

    // #[serde(rename = "ansible.builtin.file")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub file: Option<file::File>,

    // #[serde(rename = "ansible.windows.win_file")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_file: Option<win_file::WinFile>,

    // #[serde(rename = "ansible.builtin.shell")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub shell: Option<shell::Shell>,

    // #[serde(rename = "ansible.windows.win_shell")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_shell: Option<win_shell::WinShell>,

    // #[serde(rename = "ansible.builtin.unarchive")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub unarchive: Option<unarchive::Unarchive>,

    // #[serde(rename = "community.windows.win_unzip")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_unzip: Option<win_unzip::WinUnzip>,

    // #[serde(rename = "ansible.builtin.debug")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub debug: Option<debug::Debug>,

    // #[serde(rename = "ansible.builtin.user")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub user: Option<user::User>,

    // #[serde(rename = "ansible.windows.win_user")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_user: Option<win_user::WinUser>,

    // #[serde(rename = "ansible.builtin.systemd")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub systemd: Option<systemd::Systemd>,

    // #[serde(rename = "ansible.builtin.include_tasks")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub include_tasks: Option<include_tasks::IncludeTasks>,

    // #[serde(rename = "ansible.builtin.package")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub package: Option<package::Package>,

    // #[serde(rename = "ansible.windows.win_package")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_package: Option<win_package::WinPackage>,

    // #[serde(rename = "ansible.builtin.stat")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub stat: Option<stat::Stat>,

    // #[serde(rename = "ansible.windows.win_stat")]
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub win_stat: Option<win_stat::WinStat>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#loop: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub vars: Option<HashMap<String, serde_yaml::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub register: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub when: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task() {
        assert_eq!(
            serde_yaml::to_string(&Task {
                name: Some("Copy file(s)".to_string()),
                copy: Some(copy::Copy {
                    src: Some("{{ item }}".to_string()),
                    dest: "/path/to/dest/{{ item }}".to_string(),
                    mode: Some(file_mode::FileMode {
                        user: file_mode::FileModeBits::all(),
                        ..Default::default()
                    }),
                    force: Some(true),
                    ..Default::default()
                }),
                r#loop: Some("{{ files }}".to_string()),
                vars: Some(HashMap::from([(
                    "files".to_string(),
                    serde_yaml::Value::String("file.txt".to_string()),
                )])),
                ..Default::default()
            })
            .unwrap(),
            "name: Copy file(s)\nansible.builtin.copy:\n  dest: /path/to/dest/{{ item }}\n  force: true\n  mode: u=rwx,g=,o=\n  src: '{{ item }}'\nloop: '{{ files }}'\nvars:\n  files: file.txt\n",
        );
    }
}
