package profile

type SystemProfile uint

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile -linecomment -json -text -transform title-lower

const (
	SystemProfileNixos SystemProfile = iota
	SystemProfileDarwin
	SystemProfileWsl
	SystemProfileLinux
)

func (p SystemProfile) Type() string {
	return "system"
}
