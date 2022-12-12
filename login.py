import os
from dataclasses import dataclass

@dataclass
class User:
    """
    Handles authentification.
    """
    is_authenticated: bool
    is_active: bool
    is_anonymous: bool
    name: str

    def get_id(self):
        return self.name

    @staticmethod
    def get(user_id: str):
        if user_id == os.environ["MPFS_USERID"]:
            return User(
                is_authenticated=True,
                is_active=True,
                is_anonymous=False,
                name=user_id
            )
        else:
            return User(
                is_authenticated=False,
                is_active=False,
                is_anonymous=True,
                name="unlogged"
            )
