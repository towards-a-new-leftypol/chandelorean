#!/run/current-system/sw/bin/bash

set -e

# Ensure two arguments are provided: paths file and settings file
if [[ "$#" -ne 2 ]]; then
    echo "Usage: $0 PATHS_FILE SETTINGS_FILE"
    exit 1
fi

PATHS_FILE="$1"
SETTINGS_FILE="$2"

# Ensure the paths file exists
if [[ ! -f "$PATHS_FILE" ]]; then
    echo "Error: Paths file '$PATHS_FILE' not found!"
    exit 1
fi

# Ensure the settings file exists
if [[ ! -f "$SETTINGS_FILE" ]]; then
    echo "Error: Settings file '$SETTINGS_FILE' not found!"
    exit 1
fi

PROG="./dist-newstyle/build/x86_64-linux/ghc-9.4.8/chan-delorean-0.0.1/x/chan-delorean/build/chan-delorean/chan-delorean"

# Ensure chan-delorean is compiled and available
if ! command -v $PROG >/dev/null; then
    echo "Error: chan-delorean is not compiled or not in the PATH!"
    exit 1
fi

# Loop through each line of the paths file
while IFS= read -r BACKUP_PATH; do
    # Update backup_read_root in the settings file using sed
    sed -e 's|\("backup_read_root": \)".*"|\1"'"$BACKUP_PATH"'"|' "$SETTINGS_FILE" > "temp_$SETTINGS_FILE"
    
    # Run the Haskell program with the updated settings file
    time $PROG +RTS -N2 -RTS -j "temp_$SETTINGS_FILE"
done < "$PATHS_FILE"

# Optionally, remove the temporary settings file
rm "temp_$SETTINGS_FILE"
