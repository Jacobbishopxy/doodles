# @file:	spawn_proc.py
# @author:	Jacob Xie
# @date:	2024/05/17 15:28:53 Friday
# @brief:

import sys
import subprocess
import threading

from queue import Queue, Empty

ON_POSIX = "posix" in sys.builtin_module_names


def enqueue_output(out, queue):
    for line in iter(out.readline, b""):
        queue.put(line.decode().strip())
    out.close()


def main():
    print("main start")

    p = subprocess.Popen(
        "./scripts/rand_print.sh -h 3 -l 1 -m 5",
        # script_command,
        executable="/bin/bash",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        shell=True,
        close_fds=ON_POSIX,
        # universal_newlines=True,
    )

    q = Queue()
    t = threading.Thread(target=enqueue_output, args=(p.stdout, q), daemon=True)
    t.start()

    while True:
        try:
            _l = q.get()
            print(f"_l: {_l}")
            if "Exiting..." in _l:
                break
        except Empty:
            pass

    p.wait()
    print("process finished")


if __name__ == "__main__":

    main()
