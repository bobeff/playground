mod cli;
mod tasks;

use anyhow::anyhow;
use cli::{Action::*, CommandLineArgs};
use std::path::PathBuf;
use structopt::StructOpt;
use tasks::{add_task, complete_task, list_tasks, Task};

fn find_default_journal_file() -> Option<PathBuf> {
    home::home_dir().map(|mut path| {
        path.push(".rusty-journal.json");
        path
    })
}

fn main() -> anyhow::Result<()> {
    // Get the command line arguments.
    let CommandLineArgs {
        action,
        journal_file,
    } = CommandLineArgs::from_args();

    // Unpack the journal file.
    let journal_file = journal_file
        .or_else(find_default_journal_file)
        .ok_or(anyhow!("Failed to find journal file."))?;

    // Perform the action.
    match action {
        Add { task } => add_task(&journal_file, Task::new(task)),
        Done { position } => complete_task(&journal_file, position),
        List => list_tasks(&journal_file),
    }?;
    Ok(())
}
