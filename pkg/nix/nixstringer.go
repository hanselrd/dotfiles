package nix

type NixStringer interface {
	NixString() string
}
