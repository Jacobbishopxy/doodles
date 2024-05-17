# @file:	ms.py
# @author:	Jacob Xie
# @date:	2024/05/17 17:37:35 Friday
# @brief:

import os


def run_shell_cmd(cmd):
    print("run shell command = {}".format(cmd))
    os.system(cmd)


def run_shell_cmd_multithread(list_cmd, CPU_num):
    # multi-thread
    from concurrent.futures import ThreadPoolExecutor, wait, ALL_COMPLETED

    executor = ThreadPoolExecutor(max_workers=CPU_num)
    task_all = [executor.submit(run_shell_cmd, cmd) for cmd in list_cmd]
    wait(task_all, return_when=ALL_COMPLETED)
    print("ThreadPoolExecutor finished all threads")
