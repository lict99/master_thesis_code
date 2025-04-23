#!/bin/bash

# Set up strict error handling
set -e          # Exit immediately if a command exits with non-zero status
set -o pipefail # Return value of a pipeline is the value of the last command to exit with non-zero status

# Create log directory if it doesn't exist
LOG_DIR="results/log"
mkdir -p "$LOG_DIR"

# Generate timestamp for the log file
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$LOG_DIR/analysis_run_$TIMESTAMP.log"

# Function to log messages to both console and log file
log() {
    echo "$@" | tee -a "$LOG_FILE"
}

# Function to run a script based on its file extension
run_script() {
    local script="$1"
    local base_name
    base_name=$(basename "$script")

    log "==============================================="
    log "Running $base_name ($(date))"
    log "-----------------------------------------------"

    case "$script" in
    *.R)
        log "Executing with Rscript: $script"
        Rscript "$script" 2>&1 | tee -a "$LOG_FILE"
        ;;
    *.py)
        log "Executing with python: $script"
        python "$script" 2>&1 | tee -a "$LOG_FILE"
        ;;
    *.sh)
        log "Executing with bash: $script"
        bash "$script" 2>&1 | tee -a "$LOG_FILE"
        ;;
    *)
        log "ERROR: Unknown script type: $script"
        return 1
        ;;
    esac

    # Check if the command was successful
    if [ "${PIPESTATUS[0]}" -eq 0 ]; then
        log "-----------------------------------------------"
        log "SUCCESS: $base_name completed successfully"
        log "==============================================="
        return 0
    else
        log "-----------------------------------------------"
        log "ERROR: $base_name failed with exit code ${PIPESTATUS[0]}"
        log "==============================================="
        return 1
    fi
}

# Main function
main() {
    log "Starting analysis pipeline at $(date)"
    log "Log file: $LOG_FILE"

    # Find all analysis scripts with numerical prefixes
    SCRIPTS=$(find "$(dirname "$0")" -maxdepth 1 -type f \
        \( -name "[0-9][0-9]*.R" -o -name "[0-9][0-9]*.py" -o -name "[0-9][0-9]*.sh" \) | sort)

    if [ -z "$SCRIPTS" ]; then
        log "ERROR: No analysis scripts found"
        log "DEBUG: Showing all R, py, and sh files in directory:"
        find "$(dirname "$0")" -maxdepth 1 -type f \( -name "*.R" -o -name "*.py" -o -name "*.sh" \) | sort | tee -a "$LOG_FILE"
        exit 1
    fi

    log "Found $(echo "$SCRIPTS" | wc -l | tr -d ' ') scripts to run"
    log "Scripts will be run in the following order:"
    for script in $SCRIPTS; do
        log "  - $(basename "$script")"
    done
    log ""

    # Run each script in order
    for script in $SCRIPTS; do
        if ! run_script "$script"; then
            log "Pipeline stopped due to error in $(basename "$script")"
            exit 1
        fi

        log ""
    done

    log "All analysis scripts completed successfully at $(date)"
}

# Run the main function
main
