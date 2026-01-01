package nixinternal

import (
	"fmt"
	"strings"
	"time"

	"github.com/hanselrd/dotfiles/internal/hash"
)

func FakeHash(t time.Time) string {
	root := strings.Join([]string{"hanselrd", hash.Date(t), hash.TodNanoseconds(t)}, "+")
	return fmt.Sprintf("sha256-%s+%s=", root, strings.Repeat("A", 42-len(root)))
}
