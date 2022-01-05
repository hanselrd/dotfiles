import logging
import shutil
import subprocess
import time
import typing


def shell(
    cmd: str,
    input: typing.Optional[str] = None,
    progress: bool = True,
    dryrun: bool = False,
) -> typing.Tuple[int, str, str]:
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
