#!/usr/bin/env python3
"""
SimTag EPC Server - Provides tag similarity, entity extraction, and tag generation as a resident service
Uses EPC (Emacs RPC) to communicate with Emacs
"""

import os
import sys
import argparse

# Add current directory to Python path
script_dir = os.path.dirname(os.path.realpath(__file__))
if script_dir not in sys.path:
    sys.path.insert(0, script_dir)

# Try to import the SimTag module
try:
    from simtag.config import Config
    from simtag.epc_server import main as server_main
except ImportError as e:
    # If the import fails, try to display a more friendly error message
    print(f"Failed to import the SimTag module: {e}")
    print("Please make sure all dependencies are installed:")
    print("uv pip install epc sentence-transformers torch numpy requests")
    sys.exit(1)

def main():
    """Main function"""
    try:
        # Set environment variable to indicate EPC mode
        os.environ["SIMTAG_EPC_MODE"] = "1"
        
        # Parse command line arguments
        parser = argparse.ArgumentParser(description='SimTag EPC Server')
        parser.add_argument('--vector-file', help='Path to the vector file')
        parser.add_argument('--db-file', help='Path to the database file')
        parser.add_argument('--model', help='Model name')
        parser.add_argument('--debug', action='store_true', help='Enable debug mode')
        parser.add_argument('--log-file', help='Path to the log file')
        parser.add_argument('--host', default='127.0.0.1', help='Server address')
        parser.add_argument('--port', type=int, default=0, help='Server port')
        args = parser.parse_args()

        # Create a configuration object
        config = Config(
            vector_file=args.vector_file,
            db_file=args.db_file,
            model_name=args.model,
            debug=args.debug,
            log_file=args.log_file,
            host=args.host,
            port=args.port
        )
        
        # Make sure all output is flushed
        sys.stdout.flush()
        sys.stderr.flush()
        
        # Call the server main function
        server_main(config)
        
    except Exception as e:
        # Make sure error messages are written to stderr
        print(f"Failed to start the server: {e}", file=sys.stderr, flush=True)
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()