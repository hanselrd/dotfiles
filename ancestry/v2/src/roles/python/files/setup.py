from setuptools import setup

setup(
    name="dotfiles",
    version="0.1.0",
    py_modules=["dotfiles"],
    install_requires=[],
    entry_points="""
        [console_scripts]
        dotfiles=dotfiles:dotfiles
    """,
)
