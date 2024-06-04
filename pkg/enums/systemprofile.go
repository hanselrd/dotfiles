package enums

type SystemProfile uint

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile -linecomment -json -text -transform title-lower

const (
	SystemProfileNixos SystemProfile = iota
	SystemProfileDarwin
	SystemProfileLinux
	SystemProfileWsl
)

func (p SystemProfile) Type() string {
	return "system"
}

func (p SystemProfile) Profile() string {
	return p.String()
}
