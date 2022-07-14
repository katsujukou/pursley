module Pursley (main) where

import Data.Version (showVersion)
import qualified Options.Applicative as Opts
import qualified Paths_pursley as Paths
import Pursley.Command (Command (..), GlobalOptions (GlobalOptions), Options (..), program)
import qualified Pursley.Command.Init as Init
import qualified Pursley.Command.Install as Install
import qualified Pursley.Command.Setup as Setup
import qualified Pursley.Command.Update as Update
import Pursley.Console (echoShow)
import Pursley.Prelude
import Pursley.Run (withEnv)
import qualified Pursley.Types as P
import qualified RIO.Text as T
import System.Environment (getArgs)
import qualified System.IO as IO
import Text.Megaparsec (parse)

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  getArgs
    >>= Opts.handleParseResult . execParserPure parserInfo
    >>= runPursley
  where
    runPursley Options {..} = do
      withEnv optGlobalOptions (program optCommand)

    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] =
      Opts.Failure $
        Opts.parserFailure Opts.defaultPrefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    parserInfo :: Opts.ParserInfo Options
    parserInfo =
      Opts.info
        (versionInfo <*> Opts.helper <*> parser)
        ( Opts.fullDesc
            <> Opts.progDesc "Pursley - next generation PureScript tooling"
        )

    versionInfo :: Opts.Parser (a -> a)
    versionInfo =
      Opts.abortOption (Opts.InfoMsg $ showVersion Paths.version) $
        Opts.long "version"
          <> Opts.short 'v'
          <> Opts.help "Show the version number"
          <> Opts.hidden

    parser :: Opts.Parser Options
    parser = Options <$> globalOptions <*> command

    command :: Opts.Parser Command
    command =
      (Opts.hsubparser . fold)
        [ Opts.command
            "update"
            ( Opts.info
                (Update <$> updateOpts)
                (Opts.progDesc "Update the list of available packages.")
            ),
          Opts.command
            "setup"
            ( Opts.info
                (Setup <$> setupOpts)
                (Opts.progDesc "Setup pursley environment globally.")
            ),
          Opts.command
            "init"
            ( Opts.info
                (Init <$> initOpts)
                (Opts.progDesc "Create a new pursley project")
            ),
          Opts.command
            "install"
            ( Opts.info
                (Install <$> installOpts)
                (Opts.progDesc "Install packages with dependencies.")
            )
        ]

    updateOpts :: Opts.Parser Update.UpdateOptions
    updateOpts =
      Update.UpdateOptions
        <$> ( optional $
                Opts.strOption $
                  Opts.long "pin-tag"
                    <> Opts.help "Specify target revision of package-sets by git commit hash. Ignored when pin-commit option is used."
            )
        <*> ( optional $
                Opts.strOption $
                  Opts.long "pin-commit"
                    <> Opts.help "Specify target revision of package-sets by git commit hash."
            )

    setupOpts :: Opts.Parser Setup.SetupOptions
    setupOpts = do
      pinning <-
        optional $
          Opts.strOption $
            Opts.long "pinning"
              <> Opts.metavar "TAG"
              <> Opts.help "Version  of package set"
      pure $ Setup.SetupOptions pinning

    initOpts :: Opts.Parser Init.InitOptions
    initOpts = pure Init.InitOptions

    installOpts :: Opts.Parser Install.InstallOptions
    installOpts = do
      inputs <-
        many $
          Opts.argument packageNameParser $
            Opts.help "The list of packages to install"
              <> Opts.metavar "PACKAGE"
      noCache <-
        Opts.switch $
          Opts.long "no-cache"
            <> Opts.help "Install from remote and update cache"
      pure $ Install.InstallOptions inputs noCache

    globalOptions :: Opts.Parser GlobalOptions
    globalOptions =
      GlobalOptions
        <$> Opts.strOption
          ( Opts.long "cache-dir"
              <> Opts.value "~/.pursley"
              <> Opts.metavar "CACHE_DIRECTORY"
              <> Opts.help "Directory to store cache files"
          )
        <*> Opts.switch
          ( Opts.long "no-color"
              <> Opts.help "Log without ANSI color escape sequences"
          )
        <*> Opts.switch
          ( Opts.long "quiet"
              <> Opts.short 'q'
              <> Opts.help "Execute without logging"
          )

    packageNameParser :: Opts.ReadM P.PackageName
    packageNameParser = Opts.eitherReader \s ->
      let maybePkgName = hush $ parse P.packageNameParser "" (T.pack s)
       in case maybePkgName of
            Just pkgName -> pure pkgName
            _ -> Left $ "Invalid package name: " <> s