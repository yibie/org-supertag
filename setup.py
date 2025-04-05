from setuptools import setup, find_packages

setup(
    name="simtag",
    version="0.1.0",
    packages=find_packages(),
    install_requires=[
        "epc",
        "requests",
        "numpy",
        "torch",
        "sentence-transformers",
    ],
    python_requires=">=3.8",
) 