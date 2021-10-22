---
title: Assignments
headerImg: angles.jpg
---

**LATE POLICY:** You have a total of **4** late days that can be used 
in atomic units over the entire quarter. By "atomic" I mean, for example, 
that 35 mins over the deadline is equal to 1 late day.

## Virtual Machine

Those of you having trouble installing Haskell/GHC/stack etc. can use 
the [CSE 230 VM](https://drive.google.com/file/d/1zmxVhZJ3-Yfe6uBQuMnxVubTH2G_8Ax0/view?usp=sharing)

## Assignment Links

- [00-lambda](https://classroom.github.com/a/PwNVGSom), due **10/8 at 23:59:59**
- [01-trees](https://classroom.github.com/a/EKaa5k6e), due **10/22 at 23:59:59**
- [02-while](https://classroom.github.com/a/apPaNw-v), due **11/05 at 23:59:59**
 
## Aidan Denlinger's Tips on Haskell-on-ieng servers

`ieng6` instructions are the top half, working locally is the bottom half. 
This is a giant repository for a lot of common issues, so just skip to 
what you're frustrated by/interested in :)

### ieng6

## Procedure

- On Mac and Unix systems, you can use the terminal to ssh. On Windows, you can use any SSH client like [PuTTY](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html), or use [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) to run Linux commands like `ssh`. You could also use UCSD's [Linux Cloud](https://linuxcloud.ucsd.edu) with the same login as below and start a Linux SSH terminal in the browser.
- Login to ieng6 with your active directory/email login, the hostname is ieng6.ucsd.edu (ex `ssh adenling@ieng6.ucsd.edu`). If you're an extension student, you should check what your account is at [ETS Account Lookup](https://sdacs.ucsd.edu/~icc/index.php).
  - If you're getting *read-only file system* errors when running make (@16), try sshing into `ieng6-640.ucsd.edu` instead.
  - If it still isn't working, it seems that a fully local environment is the best solution so that you don't need ieng6 at all, you can find instructions for this at the bottom of the doc.
- **PREP FOR THE CLASS BY RUNNING `cs131sp21`. YOU NEED TO DO THIS EVERY TIME YOU LOG IN FOR THIS CLASS**. This is the most common mistake for ieng6 issues - do this before you do *anything else* for this class!
- If this is the only class you're using ieng6 for, you may want to consider adding `cs131sp21` to the bottom of your `~/.bashrc` file. This means that `cs131sp21` will be run automatically every time you log into ieng6. You'll want to remove this at the end of the quarter.
- From this point you'll have access to `stack`/other Haskell software and should be able to clone your repo and get started, likely using tools like `nano` or `vim` to edit your files, with the README providing instructions on how to run the test suite.

You can also use X11 forwarding on ieng6 (`ssh -X yourlogin@ieng6.ucsd.edu`) to run windowed applications like `gvim`. I think this takes extra software on Windows...

## ieng6 errors:
- *read-only file system errors when running make (@16)*: try sshing into `ieng6-640.ucsd.edu` instead. If it still isn't working, it seems that a fully local environment is the best solution so that you don't need ieng6 at all, you can find instructions for this at the bottom of the doc.

- *When running `make`, "Preparing to install GHC to an isolated location" appears*: You likely have not prepped for the class. Without prepping, the Haskell compiler tries to install itself, which will take up all your space and then fail. We're going to delete the `~/.stack` folder, which holds the compiler installation, so it will stop trying to install itself, and then ensure you're prepped so you can use the already set up compiler.
  - Log out.
  - Log in, and *don't* prep.
  - Run `rm -r .stack`, which will remove the `.stack` folder if it exists.
  - Run `ls`. If you see your PA here, you're in the wrong place: you want your PA to be in the CSE131 prepped environment. Please `cd` into it, `git push` your changes so they're saved to github, and delete the folder.
  - Now prep by running `cs131sp21`.
  - Run `rm -r .stack` again, just to make sure there is no `.stack` folder anywhere.
  - If you deleted your PA earlier, you should clone it now that we're prepped.
  - `cd` into your repo and try running `make`. It should be successful now. **From this point on, please prep before you do anything for this class any time you login.**
  -

- *permission denied (quota exceeded) errors or vim isn't saving*: You're out of space on ieng6. Run `cd` (no parameters) to go to your home directory, then run `du -hd1 | sort -hr`, which will list your folders in order of how much space they're taking up. If you see `.stack`, please follow the above issue, you shouldn't have that folder. Otherwise, clear up the space by moving or deleting the files that are taking up excessive space, then prep and try again.

- *"Device or Resource busy" when trying to delete*: (Sungwoo figured this one out last quarter, thanks!) run `lsof +D /path_to_your_repo`, which will list all processes that have files from your repo open. Find the PIDs of the processes you don't want active, and kill them using `kill pid_number` (or `kill -9 pid_number` if it refuses to close). You should now be able to delete.

# Working Locally
You don't have to use ieng6 via the terminal and tools like `nano/vim`! Here are some alternatives:
- You can use windowed applications on ieng6 as stated in the ieng6 procedure.
- Edit your files locally in whatever editor you'd like, push them to github, pull them on ieng6, and then run the test suite. This basically only uses ieng6 to compile and run your code, with all editing done locally.
- Some students have reported success with VSCode Remote Development to ssh into ieng6 and work in their VSCode, others have had major issues getting it to work, your experience may vary. If it's giving you problems that you can't solve in 10 minutes, I'd recommend either fully committing to ieng6 or a local environment (which is easier to get working than a problematic Remote Development).

## A complete local environment
Here's what you need to install for a completely local environment:
- install [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) if you're on Windows, and [here are instructions for using it with vscode](https://docs.microsoft.com/en-us/windows/wsl/tutorials/wsl-vscode). Haskell can run on Windows, but it can have really weird issues sometimes, WSL is a much more pleasant experience. (If you have an old subsystem, WSL1 might have issues, you may need to update to WSL2)
- install `stack` from a package manager like `apt` (`sudo apt install haskell-stack`) or `brew`, or from [the stack website](https://docs.haskellstack.org/) if you don't have a package manager.
- install `nasm` and `clang` for compilation (without these, running `make` will have errors saying that it couldn't find one of these programs), these should also be in package managers.
- Optionally, install IDE extensions like the VSCode `Haskell` extension for syntax highlighting/hover/more

With all this installed, you should have a local setup that seamlessly works with the PAs, just open up your IDE and run `make`.
