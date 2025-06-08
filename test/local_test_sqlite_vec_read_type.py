import sqlite3
import json
import numpy as np
import os

DB_PATH = "test_sqlite_vec_type.db"
VEC_DIM = 3 # Example dimension

def main():
    if os.path.exists(DB_PATH):
        os.remove(DB_PATH)

    conn = sqlite3.connect(DB_PATH)
    conn.enable_load_extension(True)
    
    try:
        import sqlite_vec
        sqlite_vec.load(conn)
        print(f"sqlite-vec loaded, version: {conn.execute('SELECT vec_version()').fetchone()[0]}")
    except Exception as e:
        print(f"Failed to load sqlite-vec: {e}")
        conn.close()
        return

    cursor = conn.cursor()

    # Create virtual table
    try:
        cursor.execute(f"""
        CREATE VIRTUAL TABLE IF NOT EXISTS test_float_vec USING vec0(
            embedding FLOAT[{VEC_DIM}]
        )
        """)
        print(f"Virtual table 'test_float_vec' created with dimension {VEC_DIM}.")
    except Exception as e:
        print(f"Failed to create virtual table: {e}")
        conn.close()
        return

    # Insert data as JSON string
    test_vector_list = [0.1, 0.2, 0.3]
    if len(test_vector_list) != VEC_DIM:
        print(f"Error: test_vector_list length {len(test_vector_list)} does not match VEC_DIM {VEC_DIM}")
        conn.close()
        return
        
    test_vector_json = json.dumps(test_vector_list)
    
    print(f"\nAttempting to INSERT JSON string: '{test_vector_json}' (type: {type(test_vector_json)})")
    try:
        cursor.execute("INSERT INTO test_float_vec (embedding) VALUES (?)", (test_vector_json,))
        conn.commit()
        inserted_rowid = cursor.lastrowid
        print(f"Successfully inserted vector. Row ID: {inserted_rowid}")
    except Exception as e:
        print(f"Failed to insert JSON string: {e}")
        conn.close()
        return

    # Select the data
    print(f"\nAttempting to SELECT the inserted vector (rowid {inserted_rowid})...")
    try:
        cursor.execute("SELECT embedding FROM test_float_vec WHERE rowid = ?", (inserted_rowid,))
        row = cursor.fetchone()
        if row:
            retrieved_data = row[0]
            print(f"Successfully retrieved data.")
            print(f"Retrieved data: {retrieved_data!r}")
            print(f"Type of retrieved data: {type(retrieved_data)}")

            if isinstance(retrieved_data, str):
                print("Conclusion: sqlite-vec FLOAT column returned a STRING (JSON).")
                # Try parsing
                try:
                    parsed_list = json.loads(retrieved_data)
                    print(f"Parsed JSON string: {parsed_list}")
                except Exception as e_parse:
                    print(f"Failed to parse retrieved string as JSON: {e_parse}")
            elif isinstance(retrieved_data, bytes):
                print("Conclusion: sqlite-vec FLOAT column returned BYTES.")
                # Try parsing
                try:
                    parsed_array = np.frombuffer(retrieved_data, dtype=np.float32)
                    print(f"Parsed bytes using np.frombuffer: {parsed_array.tolist()}")
                    if len(parsed_array) == VEC_DIM:
                        print(f"Length matches VEC_DIM ({VEC_DIM}).")
                    else:
                         print(f"WARNING: Length {len(parsed_array)} does NOT match VEC_DIM ({VEC_DIM}).")
                except Exception as e_parse_bytes:
                    print(f"Failed to parse retrieved bytes using np.frombuffer: {e_parse_bytes}")
            else:
                print(f"Conclusion: sqlite-vec FLOAT column returned an unexpected type: {type(retrieved_data)}")

        else:
            print("Failed to retrieve the inserted row.")
    except Exception as e:
        print(f"Failed to select data: {e}")

    conn.close()
    # os.remove(DB_PATH) # Clean up

if __name__ == '__main__':
    main() 