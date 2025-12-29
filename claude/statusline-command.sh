#!/bin/bash

# Read JSON input
input=$(cat)

# Extract values
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name')

# Shorten home directory to ~
display_dir=${cwd/#$HOME/\~}

# Get git info if in a git repo
git_info=""
if git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1; then
    branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
    if [ -n "$branch" ]; then
        if ! git -C "$cwd" --no-optional-locks diff-index --quiet HEAD -- 2>/dev/null; then
            status="✗"
        else
            status="✓"
        fi
        git_info=" on $(printf '\033[35m')$branch$(printf '\033[0m') $status"
    fi
fi

# Get context window usage
context_info=""
usage=$(echo "$input" | jq '.context_window.current_usage')
if [ "$usage" != "null" ]; then
    current=$(echo "$usage" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
    size=$(echo "$input" | jq '.context_window.context_window_size')
    if [ "$current" != "null" ] && [ "$size" != "null" ]; then
        pct=$((current * 100 / size))
        context_info=" [${pct}%]"
    fi
fi

printf "$(printf '\033[36m')%s$(printf '\033[0m')%s $(printf '\033[33m')%s$(printf '\033[0m')%s" "$display_dir" "$git_info" "$model" "$context_info"
