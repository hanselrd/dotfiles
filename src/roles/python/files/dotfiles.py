import logging
import shutil
import subprocess
import sys
import time
import typing


_LOGGER_IS_CONFIGURED: bool = False


def _configure_logger() -> None:
    global _LOGGER_IS_CONFIGURED
    if not _LOGGER_IS_CONFIGURED:
        if not logging.getLogger().hasHandlers():
            logging.basicConfig(
                format="[%(asctime)s] %(levelname)-8s | %(message)s",
                datefmt="%Y-%m-%d %H:%M:%S %z",
                level=logging.DEBUG,
                handlers=[logging.StreamHandler(sys.stdout)],
            )
        _LOGGER_IS_CONFIGURED = True


def shell(
    cmd: str,
    input: typing.Optional[str] = None,
    progress: bool = True,
    dryrun: bool = False,
) -> typing.Tuple[int, str, str]:
    """
    Execute a command in the user's default shell.
    The return code, stdout and stderr are returned as a tuple
    """

    _configure_logger()

    if dryrun:
        logging.debug(f"(DRYRUN) cmd={cmd}")
        return 0, "", ""
    else:
        logging.debug(f"cmd={cmd}")
        with subprocess.Popen(
            cmd,
            universal_newlines=True,
            shell=True,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        ) as process:
            start = time.time()
            while True:
                try:
                    stdout, stderr = process.communicate(input=input, timeout=1)
                    logging.debug(f"  rc={process.returncode}")
                    logging.debug(f"  stdout={stdout.strip()}")
                    logging.debug(f"  stderr={stderr.strip()}")
                    return process.returncode, stdout.strip(), stderr.strip()
                except subprocess.TimeoutExpired:
                    elapsed = int(time.time() - start)
                    if elapsed >= 1:
                        columns, _ = shutil.get_terminal_size()
                        progress = "=" * (elapsed % ((columns // 2) - 15))
                        print(
                            f"<{progress} {elapsed}s {progress}>".center(columns),
                            end="\r",
                            flush=True,
                        )
                    continue
