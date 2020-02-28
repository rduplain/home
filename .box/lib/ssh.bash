# ssh.bash - Utilities for Secure Shell (SSH).

ssh_agent_has_keys() {
    # Determine whether `ssh-agent` is available and has keys loaded.

    count=$(ssh-add -L 2>/dev/null | grep -i ssh | wc -l)

    if [ $count -eq 0 ]; then
        return 1
    fi

    return 0
}
