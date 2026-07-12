module Dotfiles.Scripts
  ( nixConfig
  , nixChroot
  , nixInstall
  )
where

import Control.Monad.Shell
  ( NamedLike (..)
  , Output (..)
  , Script
  , Term
  , Var
  , WithVar (..)
  , caseOf
  , cmd
  , defaultVar
  , glob
  , globalVar
  , newVarFrom
  , quote
  , static
  , takeParameter
  )
import Data.String.Utils (strip)
import qualified Data.Text.Lazy as T
import Flow

default (T.Text)

mkTmpDir :: Script (Term Var String)
mkTmpDir = do
  cmd "set" "+u"

  xdgRuntimeDir <- globalVar "XDG_RUNTIME_DIR"
  tmpDir <- globalVar "TMPDIR"
  tmpDir <- defaultVar xdgRuntimeDir =<< defaultVar tmpDir "/tmp"
  tmpDir <-
    newVarFrom
      (Output <| cmd "printf" "%s/nix-%s" tmpDir (Output <| cmd "id" "-u"))
      (NamedLike "tmpDir")

  cmd "set" "-u"
  cmd "umask" "077"
  cmd "mkdir" "-p" tmpDir
  cmd "trap" (WithVar tmpDir ("rm -rf " <>)) "EXIT"

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

nixConfig :: Script ()
nixConfig = do
  cfg <-
    newVarFrom
      ( strip
          <| unlines
            [ "experimental-features = nix-command flakes pipe-operators"
            , "show-trace = true"
            ]
      )
      (NamedLike "nixConfig")

  cmd "export" <| WithVar cfg ("NIX_CONFIG=" <>)

nixChroot :: Script ()
nixChroot = do
  cmd "set" "-euxo" "pipefail"

  type' <- takeParameter (NamedLike "type")
  dir <- takeParameter (NamedLike "dir")
  home <- globalVar "HOME"
  installDir <- newVarFrom (WithVar home (<> "/.nix-chroot")) (NamedLike "installDir")

  cmd "mkdir" "-p" installDir

  caseOf
    type'
    [
      ( quote "bindmnt"
      , do
          user <- globalVar "USER"

          cmd "sudo" "mkdir" "-pm" "0755" "/nix" dir
          cmd "sudo" "chown" user "/nix" dir
          cmd "sudo" "mount" "--bind" dir "/nix"
      )
    ,
      ( quote "nix-user-chroot"
      , do
          nixUserChrootPath <-
            newVarFrom (WithVar installDir (<> "/nix-user-chroot")) (NamedLike "nixUserChrootPath")

          cmd
            "curl"
            "-Lo"
            nixUserChrootPath
            "https://github.com/nix-community/nix-user-chroot/releases/download/2.1.1/nix-user-chroot-bin-2.1.1-x86_64-unknown-linux-musl"
          cmd "chmod" "+x" nixUserChrootPath
          cmd "mkdir" "-p" dir
          cmd nixUserChrootPath dir "bash" "-l"
      )
    ,
      ( quote "proot"
      , do
          prootPath <- newVarFrom (WithVar installDir (<> "/proot")) (NamedLike "prootPath")

          cmd "curl" "-Lo" prootPath "https://proot.gitlab.io/proot/bin/proot"
          cmd "chmod" "+x" prootPath
          cmd "mkdir" "-p" dir
          cmd prootPath "-b" (WithVar dir (<> ":/nix")) "bash"
      )
    , (glob "*", cmd "exit" (static @Int 1))
    ]

nixInstall :: Script ()
nixInstall = do
  cmd "set" "-euxo" "pipefail"

  -- version <- takeParameter (NamedLike "version")
  tmpDir <- mkTmpDir
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
