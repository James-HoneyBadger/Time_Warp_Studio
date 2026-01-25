import pytest
from sqlalchemy.ext.asyncio import create_async_engine

from time_warp.models import Base


@pytest.mark.asyncio
async def test_db_setup():
    engine = create_async_engine("sqlite+aiosqlite:///:memory:", echo=True)
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    await engine.dispose()
