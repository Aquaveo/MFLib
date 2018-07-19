"""A Buildscript for ModflowLib."""
from conan.packager import ConanMultiPackager


if __name__ == "__main__":
    # ConanPackageTools, see:
    # https://github.com/conan-io/conan-package-tools/blob/develop/README.md
    docker_setup_commands = \
        "sudo apt-get -qq update && sudo apt-get -qq install -y lcov"
    builder = ConanMultiPackager(docker_entry_script=docker_setup_commands)
    builder.add_common_builds()

    for settings, options, env_vars, build_requires, reference in \
            builder.items:
        # Require c++11 compatibility
        if settings['compiler'] == 'gcc':
            settings.update({
                'compiler.libcxx': 'libstdc++11'
            })
    builder.run()
