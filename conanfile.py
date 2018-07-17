"""The Conan package of the Modflow helper library."""
from conans import ConanFile, CMake
from conans.errors import ConanException
import os


class ModflowLibConan(ConanFile):
    """The Conan package of the Modflow helper library."""

    name = "ModflowLib"
    version = None
    license = "ModflowLib Software License"
    url = "https://github.com/Aquaveo/MFLib"
    description = "Support library for Modflow"
    settings = "os", "compiler", "build_type", "arch"
    generators = "cmake", "txt"
    build_requires = "cxxtest/4.4@aquaveo/stable"
    exports = "CMakeLists.txt", "CMakeModules/*", "LICENSE"
    exports_sources = "xmscore/*"

    def configure(self):
        """Configure the package."""
        # Raise ConanExceptions for Unsupported Versions
        s_os = self.settings.os
        s_compiler = self.settings.compiler
        s_compiler_version = self.settings.compiler.version

        if s_compiler == "apple-clang" and s_os == 'Linux':
            raise ConanException("Clang on Linux is not supported.")

        if s_compiler == "gcc" and float(s_compiler_version.value) < 5.0:
            raise ConanException("GCC < 5.0 is not supported.")

        if s_compiler == "apple-clang" and s_os == 'Macos' \
                and float(s_compiler_version.value) < 9.0:
            raise ConanException("Clang > 9.0 is required for Mac.")

    def requirements(self):
        """List all requirements."""
        self.requires("cxxtest/4.4@aquaveo/stable")
        self.requires("hdf5/1.8.1@aquaveo/stable")

    def build(self):
        """Build the package."""
        cmake = CMake(self)
        cmake.definitions["BUILD_TESTING"] = True
        cmake.configure(source_folder=".")
        cmake.build()
        print("***********(0.0)*************")
        try:
            cmake.test()
        except ConanException:
            raise
        finally:
            if os.path.isfile("TEST-cxxtest.xml"):
                with open("TEST-cxxtest.xml", "r") as f:
                    for line in f.readlines():
                        no_newline = line.strip('\n')
                        print(no_newline)
            print("***********(0.0)*************")

    def package(self):
        """List what should be copied from the build into the package."""
        # Headers
        self.copy("Export.h", dst="include/ModflowLib",
                  src="ModflowLib/source/")
        self.copy("ModflowLib.h", dst="include/ModflowLib",
                  src="ModflowLib/source/")

        # Helpers
        self.copy("*.f90", dst="fortran/", src="mfLibFortran")
        self.copy("*.f", dst="fortran/", src="mfLibFortran")

        # Libraries
        self.copy("*.lib", dst="lib", keep_path=False)
        self.copy("*.a", dst="lib", keep_path=False)
        self.copy("license", dst="licenses", ignore_case=True, keep_path=False)

    def package_info(self):
        """List details about the package, such as libraries provided."""
        if self.settings.build_type == 'Debug':
            self.cpp_info.libs = ["ModflowLib_d"]
        else:
            self.cpp_info.libs = ["ModflowLib"]
