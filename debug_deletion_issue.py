#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Debug script to analyze why delete_nodes_by_ids is not working
"""

import sqlite3
import json
import sys
import os

# Add the simtag directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'simtag'))

def debug_deletion_issue():
    """Debug the deletion issue by analyzing database content and delete IDs"""
    
    # Use the correct database path from simtag config
    db_path = os.path.expanduser("~/.emacs.d/org-supertag/supertag_vector.db")
    
    if not os.path.exists(db_path):
        print(f"❌ Database file not found: {db_path}")
        return
    
    print(f"🔍 Analyzing database: {db_path}")
    
    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        
        # 1. Check database schema
        print("\n=== DATABASE SCHEMA ===")
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = cursor.fetchall()
        print(f"Available tables: {[table[0] for table in tables]}")
        
        # 2. Check nodes table structure and content
        print("\n=== NODES TABLE ANALYSIS ===")
        try:
            cursor.execute("PRAGMA table_info(nodes)")
            columns = cursor.fetchall()
            print("Nodes table columns:", [col[1] for col in columns])
            
            cursor.execute("SELECT COUNT(*) FROM nodes")
            node_count = cursor.fetchone()[0]
            print(f"Total nodes in database: {node_count}")
            
            if node_count > 0:
                cursor.execute("SELECT node_id, type FROM nodes LIMIT 10")
                sample_nodes = cursor.fetchall()
                print("Sample nodes:")
                for node_id, node_type in sample_nodes:
                    print(f"  - {node_id} (type: {node_type})")
        except sqlite3.OperationalError as e:
            print(f"❌ Error accessing nodes table: {e}")
        
        # 3. Load hash file to check what IDs are being marked for deletion
        hash_file_path = os.path.expanduser("~/.emacs.d/org-supertag/sync_hashes.json")
        if os.path.exists(hash_file_path):
            print(f"\n=== HASH FILE ANALYSIS ===")
            with open(hash_file_path, 'r') as f:
                hash_data = json.load(f)
            
            hash_ids = list(hash_data.keys())
            print(f"Hash file contains {len(hash_ids)} IDs")
            
            # Sample hash IDs
            print("Sample hash IDs:")
            for i, hash_id in enumerate(hash_ids[:10]):
                print(f"  - {hash_id}")
            
            # 4. Check which hash IDs exist in the database
            print(f"\n=== ID MATCHING ANALYSIS ===")
            existing_ids = []
            missing_ids = []
            
            for hash_id in hash_ids[:20]:  # Check first 20 to avoid too much output
                cursor.execute("SELECT COUNT(*) FROM nodes WHERE node_id = ?", (hash_id,))
                exists = cursor.fetchone()[0] > 0
                if exists:
                    existing_ids.append(hash_id)
                else:
                    missing_ids.append(hash_id)
            
            print(f"From first 20 hash IDs:")
            print(f"  - Existing in DB: {len(existing_ids)}")
            print(f"  - Missing from DB: {len(missing_ids)}")
            
            if missing_ids:
                print("Sample missing IDs:")
                for missing_id in missing_ids[:5]:
                    print(f"  - {missing_id}")
        
        # 5. Simulate the deletion process
        print(f"\n=== DELETION SIMULATION ===")
        
        # Get some actual node IDs from the database
        cursor.execute("SELECT node_id FROM nodes LIMIT 5")
        actual_node_ids = [row[0] for row in cursor.fetchall()]
        
        if actual_node_ids:
            print(f"Testing deletion with {len(actual_node_ids)} actual node IDs...")
            
            # Test the rowid query first
            id_placeholders = ','.join('?' for _ in actual_node_ids)
            rowid_query = f"SELECT rowid, node_id FROM nodes WHERE node_id IN ({id_placeholders})"
            cursor.execute(rowid_query, actual_node_ids)
            rowid_results = cursor.fetchall()
            
            print(f"Rowid query returned {len(rowid_results)} results:")
            for rowid, node_id in rowid_results:
                print(f"  - rowid: {rowid}, node_id: {node_id}")
            
            # Simulate delete query (but don't actually delete)
            count_query = f"SELECT COUNT(*) FROM nodes WHERE node_id IN ({id_placeholders})"
            cursor.execute(count_query, actual_node_ids)
            would_delete_count = cursor.fetchone()[0]
            print(f"Would delete {would_delete_count} nodes")
        
        # 6. Check for potential ID format issues
        print(f"\n=== ID FORMAT ANALYSIS ===")
        cursor.execute("SELECT DISTINCT type FROM nodes")
        node_types = [row[0] for row in cursor.fetchall()]
        print(f"Node types in database: {node_types}")
        
        for node_type in node_types:
            cursor.execute("SELECT COUNT(*) FROM nodes WHERE type = ?", (node_type,))
            type_count = cursor.fetchone()[0]
            cursor.execute("SELECT node_id FROM nodes WHERE type = ? LIMIT 3", (node_type,))
            sample_ids = [row[0] for row in cursor.fetchall()]
            print(f"  - {node_type}: {type_count} nodes, sample IDs: {sample_ids}")
        
        conn.close()
        
    except Exception as e:
        print(f"❌ Error during analysis: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    debug_deletion_issue() 