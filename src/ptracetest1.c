
#include <assert.h>
#include <sys/ptrace.h>
#include <linux/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <linux/user.h>
#include <sys/syscall.h> 
#include <syscall.h>
#include <sys/reg.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __x86_64__
#define SYSCALL_OFF (ORIG_RAX * 8)
#else
#define SYSCALL_OFF (ORIG_EAX * 4)
#endif

void checked (char const * const s, int const r) { if (r == -1) { perror(s); abort(); } }

int main()
{
  pid_t const child = fork();

  checked("fork", child);

  if(child == 0)
  {
    checked("ptrace", ptrace(PTRACE_TRACEME, 0, NULL, NULL));
    checked("execl", execl("/usr/bin/whoami", "whoami", NULL));
  }
  
  int status;
  checked("wait", wait(&status));
  assert(WIFSTOPPED(status) && WSTOPSIG(status) == SIGTRAP);
    // No way to know whether this SIGTRAP was caused by entry/exit, because we couldn't set PTRACE_O_TRACESYSGOOD until now (see ptracetest2.c).

  printf("opaque trap passed\n");

  checked("ptrace", ptrace(PTRACE_SETOPTIONS, child, NULL, PTRACE_O_TRACESYSGOOD));

  for(;;)
  {
    checked("ptrace", ptrace(PTRACE_SYSCALL, child, NULL, NULL));

    checked("wait", wait(&status));

    if (WIFEXITED(status)) break;

    if (WSTOPSIG(status) == (SIGTRAP | 0x80))
    {
      printf("syscall trap %ld\n", ptrace(PTRACE_PEEKUSER, child, SYSCALL_OFF, NULL));
    }
    else
    {
      assert(WSTOPSIG(status) == SIGTRAP);
      printf("non-syscall trap\n");
    }
  }

  return 0;
}

// On Eelis' machines, no syscall traps for execve are shown. On CoffeeBuzz' dual core machine, one syscall trap for execve is shown.
// Conclusion: Using execve as "first SIGTRAP" foundation, one cannot know whether the first observed syscall trap is an entry or exit.

// This testcase assumes that on syscall exit traps, ORIG_EAX/ORIG_RAX still holds the syscall number.
