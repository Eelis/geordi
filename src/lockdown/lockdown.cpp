#include <seccomp.h>
#include <unistd.h>
#include <stdexcept>
#include <exception>
#include <iostream>
#include <unistd.h>
#include <utility>
#include <sys/resource.h>
#include <sys/prctl.h>
#include <signal.h>

#ifndef __x86_64__
	#error Unsupported platform
#endif

#define RULE(s,a) std::make_pair(SCMP_SYS(s), SCMP_ACT_##a)

auto rules =
	{ RULE(read,            ALLOW)
	, RULE(readv,           ALLOW)
	, RULE(pread64,         ALLOW)
	, RULE(write,           ALLOW)
	, RULE(writev,          ALLOW)
	, RULE(pwrite64,        ALLOW)
	, RULE(access,          ALLOW)
	, RULE(stat,            ALLOW)
	, RULE(fstat,           ALLOW)
	, RULE(open,            ALLOW)
	, RULE(openat,          ALLOW)
	, RULE(lseek,           ALLOW)
	, RULE(close,           ALLOW)
	, RULE(exit_group,      ALLOW)
	, RULE(execve,          ALLOW)
	, RULE(brk,             ALLOW)
	, RULE(mmap,            ALLOW)
	, RULE(mremap,          ALLOW)
	, RULE(arch_prctl,      ALLOW)
	, RULE(readlink,        ALLOW)
	, RULE(mprotect,        ALLOW)
		// Needed by glibc's malloc, though curiously only when
		// allocation fails (e.g. due to resource limits).
	, RULE(getcwd,          ALLOW)
	, RULE(gettimeofday,    ALLOW)
	, RULE(getdents,        ALLOW)
	, RULE(set_tid_address, ALLOW)
	, RULE(fallocate,       ALLOW)
	, RULE(clock_gettime,   ALLOW)
	, RULE(exit,            ALLOW)
	, RULE(sched_yield,     ALLOW)
	, RULE(lstat,           ERRNO(EPERM))
	, RULE(geteuid,         ERRNO(EPERM))
	, RULE(gettid,          ERRNO(EPERM))
	, RULE(getpid,          ERRNO(EPERM))
	, RULE(getrusage,       ERRNO(EPERM))
	, RULE(socket,          ERRNO(EPERM))
	, RULE(getrlimit,       ERRNO(EPERM))
	, RULE(tgkill,          ERRNO(EPERM))
	, RULE(ioctl,           ERRNO(EPERM))
	, RULE(chmod,           ERRNO(EPERM))
	, RULE(shmdt,           ERRNO(EPERM))
	, RULE(sysinfo,         ERRNO(EPERM))
	, RULE(futex,           ERRNO(0))
	, RULE(madvise,         ERRNO(0))
	, RULE(fstatfs,         ERRNO(0))
	, RULE(fcntl,           ERRNO(0))
	, RULE(mkdir,           ERRNO(0))
	, RULE(symlink,         ERRNO(0))
	, RULE(setitimer,       ERRNO(0))
	, RULE(unlink,          ERRNO(0))
	, RULE(dup2,            ERRNO(0))
	, RULE(munmap,          ERRNO(0))
	, RULE(umask,           ERRNO(0))
	, RULE(vfork,           ERRNO(0))
	, RULE(rt_sigprocmask,  ERRNO(0))
	, RULE(set_robust_list, ERRNO(0))
	, RULE(sigaltstack,     ERRNO(0))
	};

void e(char const * const what) { throw std::runtime_error(what); }

void limitResource(int const resource, rlim_t const soft, rlim_t const hard)
{
	rlimit const rl = { soft, hard };
	if (setrlimit(resource, &rl) != 0) e("setrlimit");
}

void limitResource(int const resource, rlim_t const l)
{
	limitResource(resource, l, l);
}

int main(int const argc, char * const * const argv)
{
	try
	{
		alarm(10);

		close(fileno(stdin));
		for (int fd = fileno(stderr); fd != 1024; ++fd) close(fd);
		dup2(fileno(stdout), fileno(stderr));

		limitResource(RLIMIT_CPU, 9, 11);
		limitResource(RLIMIT_AS, 512*1024*1024);
		limitResource(RLIMIT_DATA, 512*1024*1024);
		limitResource(RLIMIT_FSIZE, 10*1024*1024);
		limitResource(RLIMIT_LOCKS, 0);
		limitResource(RLIMIT_MEMLOCK, 0);
		limitResource(RLIMIT_NPROC, 16);

		if (argc < 2) e("params: program args");

		if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) e("prctl");

		scmp_filter_ctx const ctx = seccomp_init(SCMP_ACT_TRAP);

		if (!ctx) e("seccomp_init");

		if (seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(clone), 1,
			SCMP_CMP(0, SCMP_CMP_MASKED_EQ, CLONE_THREAD, CLONE_THREAD)) != 0)
			e("seccomp_rule_add");

		if (seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(rt_sigaction), 1,
			SCMP_CMP(0, SCMP_CMP_NE, SIGALRM)) != 0)
			e("seccomp_rule_add");

		for (auto && p : rules)
			if (seccomp_rule_add(ctx, p.second, p.first, 0) != 0)
				e("seccomp_rule_add");

		if (seccomp_load(ctx) < 0) e("seccomp_load");

		execv(argv[1], argv + 1);
		e("execv");
	}
	catch (std::exception const & e)
	{
		std::cerr << "exception: " << e.what() << '\n';
		return 1;
	}
}
