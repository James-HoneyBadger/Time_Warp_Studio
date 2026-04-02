"""
User authentication and role management for HB Banking ERP
"""
from ..data.erp_data import get_connection, hash_password as hp


def authenticate(username, password):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT id, role FROM users WHERE username=? AND password_hash=?", (username, hp(password)))
        return cur.fetchone()  # (id, role) or None


def add_user(username, password, role, email=""):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO users (username, password_hash, role, email) VALUES (?, ?, ?, ?)",
                    (username, hp(password), role, email))
        conn.commit()
        return cur.lastrowid


def change_password(user_id, old_password, new_password):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT password_hash FROM users WHERE id=?", (user_id,))
        row = cur.fetchone()
        if row and row[0] == hp(old_password):
            cur.execute("UPDATE users SET password_hash=? WHERE id=?", (hp(new_password), user_id))
            conn.commit()
            return True
    return False


def get_user_permissions(role):
    """Return permissions based on role."""
    permissions = {
        'admin': ['read', 'write', 'delete', 'admin'],
        'teller': ['read', 'write'],
        'auditor': ['read'],
        'user': ['read']
    }
    return permissions.get(role, ['read'])


if __name__ == "__main__":
    # Test authentication
    result = authenticate("admin", "admin123")
    print("Auth result:", result)
