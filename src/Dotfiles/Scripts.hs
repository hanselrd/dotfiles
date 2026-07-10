module Dotfiles.Scripts
  ( nixConfig
  , nixBindMount
  , nixInstall
  , chroot
  )
where

import Control.Monad (forM_)
import Control.Monad.Shell
  ( NamedLike (..)
  , Output (..)
  , Script
  , Term
  , Var
  , WithVar (..)
  , cmd
  , defaultVar
  , globalVar
  , newVarFrom
  , takeParameter
  )
import qualified Data.Text.Lazy as T
import Flow

default (T.Text)

nixConfig :: Script ()
nixConfig = do
  cfg <-
    newVarFrom
      "experimental-features = nix-command flakes pipe-operators\nshow-trace = true"
      (NamedLike "nixConfig")

  cmd "export" <| WithVar cfg ("NIX_CONFIG=" <>)

mkTmpDir :: Script (Term Var String)
mkTmpDir = do
  xdgRuntimeDir <- globalVar "XDG_RUNTIME_DIR"
  tmpDir <- globalVar "TMPDIR"
  tmpDir <- defaultVar xdgRuntimeDir =<< defaultVar tmpDir "/tmp"
  tmpDir <-
    newVarFrom
      (Output <| cmd "printf" "%s/nix-%s" tmpDir (Output <| cmd "id" "-u"))
      (NamedLike "tmpDir")

  cmd "umask" "077"
  cmd "mkdir" "-p" tmpDir

  return tmpDir

data Temp = TempDir | TempFile
  deriving (Eq, Show)

mkTemp :: Term Var String -> String -> Temp -> Script (Term Var String)
mkTemp tmpDir template temp = do
  template <-
    newVarFrom
      (Output <| cmd "printf" "%s/%s" tmpDir template)
      (NamedLike "template")

  newVarFrom
    ( Output <| case temp of
        TempDir -> cmd "mktemp" "-d" template
        TempFile -> cmd "mktemp" template
    )
    ( NamedLike <| case temp of
        TempDir -> "tempDir"
        TempFile -> "tempFile"
    )

nixBindMount :: Script ()
nixBindMount = do
  cmd "set" "-euxo" "pipefail"

  dir <- takeParameter (NamedLike "dir")
  user <- globalVar "USER"

  cmd "sudo" "mkdir" "-pm" "0755" "/nix" dir
  cmd "sudo" "chown" user "/nix" dir
  cmd "sudo" "mount" "--bind" dir "/nix"

nixInstall :: Script ()
nixInstall = do
  cmd "set" "-euxo" "pipefail"

  -- version <- takeParameter (NamedLike "version")
  tmpDir <- mkTmpDir

  cmd "trap" (WithVar tmpDir ("rm -rf " <>)) "EXIT"

  tempDir <- mkTemp tmpDir "nix-install.XXXXXX" TempDir
  installPath <- newVarFrom (WithVar tempDir (<> "/install.sh")) (NamedLike "installPath")

  -- cmd
  --   "curl"
  --   "-Lo"
  --   installPath
  --   (Output <| cmd "printf" "https://releases.nixos.org/nix/nix-%s/install" version)
  cmd "curl" "-Lo" installPath "https://nixos.org/nix/install"
  cmd "chmod" "+x" installPath
  cmd installPath "--no-daemon"

chroot :: Script ()
chroot = do
  cmd "set" "-euxo" "pipefail"

  home <- globalVar "HOME"
  installDir <- newVarFrom (WithVar home (<> "/.bootstrap")) (NamedLike "installDir")
  nixUserChrootPath <-
    newVarFrom (WithVar installDir (<> "/nix-user-chroot")) (NamedLike "nixUserChrootPath")
  prootPath <- newVarFrom (WithVar installDir (<> "/proot")) (NamedLike "prootPath")

  let chroots =
        [
          ( nixUserChrootPath
          , "https://github.com/nix-community/nix-user-chroot/releases/download/2.1.1/nix-user-chroot-bin-2.1.1-x86_64-unknown-linux-musl"
          )
        , (prootPath, "https://proot.gitlab.io/proot/bin/proot")
        ]

  forM_
    chroots
    <| \(installPath, url) -> do
      cmd "curl" "-Lo" installPath url
      cmd "chmod" "+x" installPath
