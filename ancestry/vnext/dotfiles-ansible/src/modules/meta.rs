use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FreeForm {
    ClearFacts,
    ClearHostErrors,
    EndBatch,
    EndHost,
    EndPlay,
    FlushHandlers,
    #[default]
    Noop,
    RefreshInventory,
    ResetConnection,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Meta(pub FreeForm);
