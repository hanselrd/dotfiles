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
