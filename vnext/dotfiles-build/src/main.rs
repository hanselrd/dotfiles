use std::path::PathBuf;

fn main() {
    println!("{:#?}", *dfc::globals::CONFIG);

    println!(
        "{:#?}",
        Vec::<dfa::Task>::from(dfc::Command::CreateDirectories {
            paths: vec![PathBuf::from("/tmp1"), PathBuf::from("/tmp2")]
        })
    );

    println!(
        "{:#?}",
        Vec::<dfa::Task>::from(dfc::Command::CopyFiles {
            sources: vec![PathBuf::from("item1.txt"), PathBuf::from("item2.txt")],
            destination: dfc::globals::CONFIG.user_temp_dir.clone(),
            mode: Some(dfa::types::FileMode {
                user: dfa::types::FileModeBits::all(),
                ..Default::default()
            })
        })
    );
}
