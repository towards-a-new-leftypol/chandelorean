from setuptools import setup

requires = [
    'aiohttp',
    'aiofiles',
    'aiodns'
]

setup(
    name='chandelorean',
    version='0.0.1',
    description="Imageboard style forum that lets you time travel. Meant to be an archive.",
    classifiers=[
        "Programming Language :: Python",
        "Topic :: Internet :: WWW/HTTP",
    ],
    author='Zer0-',
    author_email='paul_cockshott@protonmail.com',
    packages=['chandelorean'],
    include_package_data=True,
    install_requires=requires,
)
