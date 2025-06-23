#!/usr/bin/env python3
"""
Database Unlock Utility for SimTag

This script helps unlock a SQLite database that may be locked due to:
- Crashed processes that didn't properly close connections
- WAL files left behind from previous sessions
- Multiple processes accessing the same database

Usage:
    python unlock_database.py [database_path]

If no database path is provided, it will look for the default database
in the data directory.
"""

import os
import sys
import sqlite3
import argparse
from pathlib import Path


def find_default_database():
    """Find the default database path."""
    # Look for common database locations
    possible_paths = [
        "../data/org_supertag.db",
        "data/org_supertag.db", 
        os.path.expanduser("~/.emacs.d/org-supertag/org_supertag.db"),
        os.path.expanduser("~/Documents/emacs/package/org-supertag/data/org_supertag.db")
    ]
    
    for path in possible_paths:
        if os.path.exists(path):
            return path
    
    return None


def check_database_status(db_path):
    """Check if database is accessible."""
    try:
        conn = sqlite3.connect(db_path, timeout=5.0)
        conn.execute("SELECT 1").fetchone()
        conn.close()
        return True, "Database is accessible"
    except sqlite3.OperationalError as e:
        return False, str(e)
    except Exception as e:
        return False, f"Unexpected error: {e}"


def force_unlock_database(db_path):
    """Force unlock database by removing WAL and SHM files."""
    print(f"Attempting to unlock database: {db_path}")
    
    # Check if database file exists
    if not os.path.exists(db_path):
        print(f"❌ Database file not found: {db_path}")
        return False
    
    # Check current status
    accessible, status = check_database_status(db_path)
    if accessible:
        print(f"✅ Database is already accessible: {status}")
        return True
    
    print(f"⚠️  Database issue detected: {status}")
    
    # Look for WAL and SHM files
    wal_file = db_path + "-wal"
    shm_file = db_path + "-shm"
    
    files_removed = []
    
    try:
        if os.path.exists(wal_file):
            os.remove(wal_file)
            files_removed.append("WAL")
            print(f"🗑️  Removed WAL file: {wal_file}")
            
        if os.path.exists(shm_file):
            os.remove(shm_file)
            files_removed.append("SHM")
            print(f"🗑️  Removed SHM file: {shm_file}")
            
        if not files_removed:
            print("ℹ️  No WAL/SHM files found to remove")
            
        # Test database access again
        accessible, status = check_database_status(db_path)
        if accessible:
            print(f"✅ Database unlocked successfully: {status}")
            return True
        else:
            print(f"❌ Database still not accessible: {status}")
            return False
            
    except Exception as e:
        print(f"❌ Error during unlock process: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(
        description="Unlock SQLite database for SimTag",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python unlock_database.py
    python unlock_database.py /path/to/database.db
    python unlock_database.py --check-only /path/to/database.db
        """
    )
    
    parser.add_argument(
        'database_path', 
        nargs='?', 
        help='Path to the SQLite database file'
    )
    
    parser.add_argument(
        '--check-only', 
        action='store_true',
        help='Only check database status, do not attempt to unlock'
    )
    
    args = parser.parse_args()
    
    # Determine database path
    db_path = args.database_path
    if not db_path:
        db_path = find_default_database()
        if not db_path:
            print("❌ No database path provided and no default database found.")
            print("Please specify the database path:")
            print("    python unlock_database.py /path/to/database.db")
            return 1
        else:
            print(f"ℹ️  Using default database: {db_path}")
    
    # Check if database exists
    if not os.path.exists(db_path):
        print(f"❌ Database file not found: {db_path}")
        return 1
    
    print(f"🔍 Checking database: {db_path}")
    
    # Check current status
    accessible, status = check_database_status(db_path)
    print(f"Status: {status}")
    
    if args.check_only:
        return 0 if accessible else 1
    
    if accessible:
        print("✅ Database is already accessible, no action needed.")
        return 0
    
    # Attempt to unlock
    if "database is locked" in status.lower():
        success = force_unlock_database(db_path)
        return 0 if success else 1
    else:
        print(f"❌ Database issue is not lock-related: {status}")
        print("This tool can only help with database lock issues.")
        return 1


if __name__ == "__main__":
    sys.exit(main()) 