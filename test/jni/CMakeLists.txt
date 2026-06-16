# Full JNI load test: compile a minimal hec.heclib.util.Heclib (matching the JNI symbol
# naming) and run it on a real JVM that System.load()s the built javaHeclib and calls a
# native method. This proves the published JNI native loads into a matching-arch JVM,
# its dependencies resolve, and its exported JNI symbols bind -- i.e. the JNI artifact
# works through the boundary a Java consumer actually uses.
set(_jni_classes "${CMAKE_CURRENT_BINARY_DIR}/classes")

add_custom_command(
    OUTPUT "${_jni_classes}/hec/heclib/util/Heclib.class"
    COMMAND "${Java_JAVAC_EXECUTABLE}" -d "${_jni_classes}" "${CMAKE_CURRENT_SOURCE_DIR}/Heclib.java"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/Heclib.java"
    COMMENT "Compiling JNI load-test harness"
)
add_custom_target(jni_loadtest_harness ALL
    DEPENDS "${_jni_classes}/hec/heclib/util/Heclib.class")
add_dependencies(jni_loadtest_harness javaHeclib)

add_test(
    NAME jni_load
    COMMAND "${Java_JAVA_EXECUTABLE}" -cp "${_jni_classes}"
            hec.heclib.util.Heclib "$<TARGET_FILE:javaHeclib>"
)
