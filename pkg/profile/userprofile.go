package profile

type UserProfile uint

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile -linecomment -json -text -transform title-lower

const (
	UserProfileBase UserProfile = iota
	UserProfileStandard
	UserProfileMinimal
	UserProfileFull
)

func (p UserProfile) Type() string {
	return "user"
}
