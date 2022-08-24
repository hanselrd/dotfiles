use crate::*;
use serde::Serialize;
use serde_yaml;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Default, Clone, PartialEq, Serialize)]
pub struct Task {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#become: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub become_user: Option<String>,

    #[serde(rename = "ansible.builtin.copy")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub copy: Option<modules::Copy>,

    #[serde(rename = "ansible.windows.win_copy")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_copy: Option<modules::WinCopy>,

    #[serde(rename = "ansible.builtin.file")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<modules::File>,

    #[serde(rename = "ansible.windows.win_file")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_file: Option<modules::WinFile>,

    #[serde(rename = "ansible.builtin.shell")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shell: Option<modules::Shell>,

    #[deprecated]
    #[serde(rename = "ansible.windows.win_shell")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_shell: Option<modules::WinShell>,

    #[serde(rename = "ansible.windows.win_powershell")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_powershell: Option<modules::WinPowershell>,

    #[serde(rename = "ansible.builtin.unarchive")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unarchive: Option<modules::Unarchive>,

    #[serde(rename = "community.windows.win_unzip")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_unzip: Option<modules::WinUnzip>,

    #[serde(rename = "ansible.builtin.debug")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug: Option<modules::Debug>,

    #[serde(rename = "ansible.builtin.user")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user: Option<modules::User>,

    #[serde(rename = "ansible.windows.win_user")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_user: Option<modules::WinUser>,

    #[serde(rename = "ansible.builtin.systemd")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub systemd: Option<modules::Systemd>,

    #[serde(rename = "ansible.builtin.include_tasks")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_tasks: Option<modules::IncludeTasks>,

    #[serde(rename = "ansible.builtin.package")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<modules::Package>,

    #[serde(rename = "ansible.windows.win_package")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_package: Option<modules::WinPackage>,

    #[serde(rename = "chocolatey.chocolatey.win_chocolatey")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_chocolatey: Option<modules::WinChocolatey>,

    #[serde(rename = "ansible.builtin.stat")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stat: Option<modules::Stat>,

    #[serde(rename = "ansible.windows.win_stat")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_stat: Option<modules::WinStat>,

    #[serde(rename = "ansible.builtin.meta")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub meta: Option<modules::Meta>,

    #[serde(rename = "ansible.builtin.ping")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ping: Option<modules::Ping>,

    #[serde(rename = "ansible.windows.win_ping")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub win_ping: Option<modules::WinPing>,

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
                copy: Some(modules::Copy {
                    src: Some(PathBuf::from("{{ item }}")),
                    dest: PathBuf::from("/path/to/dest/{{ item }}"),
                    mode: Some(types::FileMode {
                        user: types::FileModeBits::all(),
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
        assert_eq!(
            serde_yaml::to_string(&Task {
                meta: Some(modules::Meta(modules::meta::FreeForm::EndPlay)),
                ..Default::default()
            })
            .unwrap(),
            "ansible.builtin.meta: end_play\n"
        );
    }
}
