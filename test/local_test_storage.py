# local_test_storage.py
import logging
import sys
import os

# --- Logging Setup ---
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(message)s',
    handlers=[logging.StreamHandler(sys.stdout)]
)
logging.info("LOCAL TEST: Logging configured.")

# --- Adjust sys.path to find the 'simtag' module ---
# Assuming this script (local_test_storage.py) is INSIDE the 'simtag' directory (e.g., project_root/simtag/local_test_storage.py)
# And 'core' is a subdirectory of 'simtag' (e.g., project_root/simtag/core/storage.py)
# We need to add the PARENT directory of 'simtag' (the project root) to sys.path.
current_script_path = os.path.abspath(__file__)
# simtag_directory_path will be the directory containing this script, e.g., /path/to/project/simtag
simtag_directory_path = os.path.dirname(current_script_path)
# project_root_path will be the parent of simtag_directory_path, e.g., /path/to/project
project_root_path = os.path.dirname(simtag_directory_path)

if project_root_path not in sys.path:
    sys.path.insert(0, project_root_path)
    logging.info(f"LOCAL TEST: Added project root {project_root_path} to sys.path")

try:
    logging.info("LOCAL TEST: Attempting to import VectorStorage...")
    from simtag.core.storage import VectorStorage
    logging.info("LOCAL TEST: VectorStorage imported.")

    # Define a dummy database path for this local test
    # Placing dummy_db in the same directory as this script for simplicity
    dummy_db_path = os.path.join(simtag_directory_path, "dummy_supertag_vector.db")
    logging.info(f"LOCAL TEST: Attempting to instantiate VectorStorage with db: {dummy_db_path}")

    vs_instance = VectorStorage(db_path=dummy_db_path)
    logging.info("LOCAL TEST: VectorStorage instantiated.")

    if hasattr(vs_instance, 'has_vector_ext') and vs_instance.has_vector_ext:
        logging.info(f"LOCAL TEST: VectorStorage reports sqlite-vec IS available. Version: {vs_instance.check_vec_extension()}")
    else:
        logging.info("LOCAL TEST: VectorStorage reports sqlite-vec IS NOT available or check_vec_extension failed.")

except ImportError as e:
    logging.error(f"LOCAL TEST: ImportError: {e}. Check Python path and module structure. Current sys.path: {sys.path}", exc_info=True)
except Exception as e:
    logging.error(f"LOCAL TEST: An unexpected error occurred: {e}", exc_info=True)
