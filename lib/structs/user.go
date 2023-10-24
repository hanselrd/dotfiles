package structs

type User struct {
	UserName        string `json:"username"`
	Name            string `json:"name"`
	Email           string `json:"email"`
	HomeDirectory   string `json:"homeDirectory"`
	ConfigDirectory string `json:"configDirectory"`
	CacheDirectory  string `json:"cacheDirectory"`
}
