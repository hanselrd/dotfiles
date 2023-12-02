package enums

type UserProfile uint

//go:generate stringer -type UserProfile -linecomment
//go:generate go run codegen.go UserProfile

const (
	UserProfileBase     UserProfile = iota // base
	UserProfileStandard                    // standard
	UserProfileMinimal                     // minimal
	UserProfileFull                        // full
)

func (p UserProfile) Type() string {
	return "user"
}

func (p UserProfile) Profile() string {
	return p.String()
}
