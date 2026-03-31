import os
import shutil
from typing import List

def install_language_pack(pack_name: str, source_dir: str, target_dir: str) -> None:
    """Install a new language pack by copying its files to the target directory."""
    pack_path = os.path.join(source_dir, pack_name)
    if not os.path.exists(pack_path):
        raise FileNotFoundError(f"Language pack '{pack_name}' not found in {source_dir}.")

    target_path = os.path.join(target_dir, pack_name)
    if os.path.exists(target_path):
        raise FileExistsError(f"Language pack '{pack_name}' is already installed.")

    shutil.copytree(pack_path, target_path)
    print(f"✅ Language pack '{pack_name}' installed successfully.")

def list_available_packs(source_dir: str) -> List[str]:
    """List all available language packs in the source directory."""
    return [d for d in os.listdir(source_dir) if os.path.isdir(os.path.join(source_dir, d))]

def list_installed_packs(target_dir: str) -> List[str]:
    """List all installed language packs in the target directory."""
    return [d for d in os.listdir(target_dir) if os.path.isdir(os.path.join(target_dir, d))]

if __name__ == "__main__":
    SOURCE_DIR = "./language_packs"
    TARGET_DIR = "./installed_languages"

    # Example usage
    print("Available language packs:", list_available_packs(SOURCE_DIR))
    print("Installed language packs:", list_installed_packs(TARGET_DIR))

    try:
        install_language_pack("example_pack", SOURCE_DIR, TARGET_DIR)
    except (FileNotFoundError, FileExistsError) as e:
        print(f"❌ {e}")