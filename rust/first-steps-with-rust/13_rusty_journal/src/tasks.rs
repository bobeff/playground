use anyhow::{anyhow, Context, Result};
use chrono::{serde::ts_seconds, DateTime, Local, Utc};
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    fmt::{Display, Formatter},
    fs::{File, OpenOptions},
    io::{Seek, SeekFrom},
    path::PathBuf,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Task {
    pub text: String,
    #[serde(with = "ts_seconds")]
    pub created_at: DateTime<Utc>,
}

impl Task {
    pub fn new(text: String) -> Self {
        let created_at = Utc::now();
        Task { text, created_at }
    }
}

fn collect_tasks(mut file: &File) -> Result<Vec<Task>> {
    // Rewind the file before reading from it.
    file.seek(SeekFrom::Start(0))?;
    let tasks: Vec<Task> = match serde_json::from_reader(file) {
        Ok(tasks) => tasks,
        Err(e) if e.is_eof() => Vec::new(),
        Err(e) => Err(e)?,
    };
    // Rewind the file after reading from it.
    file.seek(SeekFrom::Start(0))?;
    Ok(tasks)
}

pub fn add_task(journal_path: &PathBuf, task: Task) -> Result<()> {
    // Open the file.
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(journal_path)
        .with_context(|| format!("Cannot open {:?}.", journal_path))?;

    // Consume the file's contents as a vector of tasks.
    let mut tasks = collect_tasks(&file)?;

    // Write the modified task list back into the file.
    tasks.push(task);
    serde_json::to_writer_pretty(&file, &tasks)?;

    Ok(())
}

pub fn complete_task(journal_path: &PathBuf, position: usize) -> Result<()> {
    // Open the file.
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(journal_path)
        .with_context(|| format!("Cannot open {:?}.", journal_path))?;

    // Consume the file's contents as a vector of tasks.
    let mut tasks = collect_tasks(&file)?;

    // Try to remove the task at `position` from the vector.
    if position == 0 || position > tasks.len() {
        return Err(anyhow!("Invalid task ID {}", position));
    }
    tasks.remove(position - 1);

    // Truncate the file.
    file.set_len(0)?;

    // Write the modified task list back to the file.
    serde_json::to_writer_pretty(&file, &tasks)?;

    Ok(())
}

pub fn list_tasks(journal_path: &PathBuf) -> Result<()> {
    // Open the file.
    let file = OpenOptions::new()
        .read(true)
        .open(journal_path)
        .with_context(|| format!("Cannot open {:?}.", journal_path))?;

    // Parse the file and collect the tasks.
    let tasks = collect_tasks(&file)?;

    // Enumerate and display the tasks if any.
    if tasks.is_empty() {
        println!("The task list is empty.");
    } else {
        for (id, task) in tasks.iter().enumerate() {
            println!("{}: {}", id + 1, task);
        }
    }

    Ok(())
}

impl Display for Task {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let created_at = self.created_at.with_timezone(&Local).format("%F %H:%M");
        write!(formatter, "{:<50} [{}]", self.text, created_at)
    }
}
