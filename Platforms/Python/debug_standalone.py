import asyncio

from sqlalchemy.ext.asyncio import create_async_engine

from time_warp.models import Base


async def main():
    print("Creating engine...")
    engine = create_async_engine("sqlite+aiosqlite:///:memory:", echo=True)
    print("Beginning connection...")
    try:
        async with engine.begin() as conn:
            print("Creating tables...")
            await conn.run_sync(Base.metadata.create_all)
            print("Tables created.")
    except Exception as e:
        print(f"Error: {e}")
    finally:
        await engine.dispose()
        print("Done.")


if __name__ == "__main__":
    asyncio.run(main())
