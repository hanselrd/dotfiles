package structs

import (
	"fmt"
)

type User struct {
	UserName string `json:"username"`
	Name     string `json:"name"`
	Email    string `json:"email"`
	UserOpts
}

type UserOpts struct {
	HomeDirectory   string `json:"homeDirectory"`
	ConfigDirectory string `json:"configDirectory"`
	CacheDirectory  string `json:"cacheDirectory"`
	DataDirectory   string `json:"dataDirectory"`
	StateDirectory  string `json:"stateDirectory"`
}

type UserOptFn func(*UserOpts)

func WithHomeDirectory(homeDirectory string, cascade bool) UserOptFn {
	return func(o *UserOpts) {
		o.HomeDirectory = homeDirectory
		if cascade {
			o.ConfigDirectory = fmt.Sprintf("%v/.config", homeDirectory)
			o.CacheDirectory = fmt.Sprintf("%v/.cache", homeDirectory)
			o.DataDirectory = fmt.Sprintf("%v/.local/share", homeDirectory)
			o.StateDirectory = fmt.Sprintf("%v/.local/state", homeDirectory)
		}
	}
}

func defaultUserOpts(userName string) (o UserOpts) {
	WithHomeDirectory(
		fmt.Sprintf("/home/%v", userName),
		true,
	)(&o)
	return
}

func NewUser(userName string, name string, email string, opts ...UserOptFn) (u *User) {
	u = new(User)
	u.UserName = userName
	u.Name = name
	u.Email = email
	u.UserOpts = defaultUserOpts(userName)
	for _, fn := range opts {
		fn(&u.UserOpts)
	}
	return
}
