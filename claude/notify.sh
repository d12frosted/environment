#!/bin/bash

# Find the terminal window running this Claude session by PID
space_info=""
session_title=""
repo_name=""

# Get git repository name if in a repo
if git rev-parse --is-inside-work-tree &>/dev/null; then
    repo_root=$(git rev-parse --show-toplevel 2>/dev/null)
    if [ -n "$repo_root" ]; then
        repo_name=$(basename "$repo_root")
    fi
fi

if command -v yabai &> /dev/null; then
    # Get the terminal's PID (walk up process tree to find terminal app)
    term_pid=$$
    while [ "$term_pid" -gt 1 ]; do
        parent_pid=$(ps -o ppid= -p "$term_pid" 2>/dev/null | tr -d ' ')
        [ -z "$parent_pid" ] && break
        parent_name=$(ps -o comm= -p "$parent_pid" 2>/dev/null)
        # Check if parent is a known terminal app
        if [[ "$parent_name" =~ (Terminal|iTerm|Alacritty|kitty|WezTerm|Ghostty) ]]; then
            term_pid=$parent_pid
            break
        fi
        term_pid=$parent_pid
    done

    # Find the window with matching PID
    win_json=$(yabai -m query --windows 2>/dev/null | jq --arg pid "$term_pid" '[.[] | select(.pid == ($pid | tonumber))] | .[0] // empty')

    if [ -n "$win_json" ] && [ "$win_json" != "null" ]; then
        # Get space index for this window
        space_idx=$(echo "$win_json" | jq -r '.space')
        if [ -n "$space_idx" ] && [ "$space_idx" != "null" ]; then
            # Get space label
            space_label=$(yabai -m query --spaces 2>/dev/null | jq -r --arg idx "$space_idx" '.[] | select(.index == ($idx | tonumber)) | .label // empty')
            if [ -n "$space_label" ]; then
                space_info="${space_idx}:${space_label}"
            else
                space_info="space ${space_idx}"
            fi
        fi

        # Get window title
        title=$(echo "$win_json" | jq -r '.title // empty')
        if [ -n "$title" ]; then
            if [ ${#title} -gt 40 ]; then
                session_title="${title:0:37}..."
            else
                session_title="$title"
            fi
        fi
    fi
fi

# Message is just the input
message="$1"

# Build subtitle from space info and repo
subtitle=""
if [ -n "$space_info" ]; then
    subtitle="$space_info"
fi
if [ -n "$repo_name" ]; then
    if [ -n "$subtitle" ]; then
        subtitle="${subtitle} ${repo_name}"
    else
        subtitle="$repo_name"
    fi
fi

# Send notification using terminal-notifier with Claude as sender if available
if command -v terminal-notifier &>/dev/null; then
    if [ -n "$subtitle" ]; then
        terminal-notifier -title "Claude Code" -subtitle "$subtitle" -message "$message" -sender com.anthropic.claudefordesktop
    else
        terminal-notifier -title "Claude Code" -message "$message" -sender com.anthropic.claudefordesktop
    fi
else
    # Fallback to osascript
    if [ -n "$subtitle" ]; then
        osascript -e "display notification \"${message}\" with title \"Claude Code\" subtitle \"${subtitle}\""
    else
        osascript -e "display notification \"${message}\" with title \"Claude Code\""
    fi
fi
