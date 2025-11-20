if(JNI_FOUND) 
    message(STATUS "Using JDK -> ${JAVA_JVM_LIBRARY}")

    add_library(javaHeclib SHARED)

    if( ${CMAKE_C_COMPILER_ID} MATCHES GNU)    
        target_compile_options(javaHeclib PRIVATE -fPIC -std=gnu99)
    elseif(${CMAKE_C_COMPILER_ID} MATCHES SunPro)
        target_compiler_options(javaHecib PRIVATE -m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs)
    else()
        message(STATUS "generic options")
    endif()
    target_include_directories(javaHeclib PUBLIC src/headers ${JNI_INCLUDE_DIRS})
    target_link_libraries(javaHeclib PRIVATE heclib)

    target_sources(javaHeclib PRIVATE
src/Hec_zrecInfo.c
src/Hec_zcheckAccessReset.c
src/Hec_zcatalog.c
src/Hec_zckpat.c
src/Hec_zaliasAdd.c
src/Hec_openLogFile.c
src/Hec_zpdRetrieve.c
src/C_CatalogToJava.c
src/Hec_zfileAccessInfo.c
src/jni_utility.h
src/Hec_ztextStore.c
src/Hec_zdbmod.c
src/Hec_zwhatChangedSetStart.c
src/Hec_zpdStore.c
src/Hec_zundel.c
src/Hec_zaliasRemove.c
src/Hec_zfilst.c
src/Hec_zckmul.c
src/Hec_zcorec.c
src/Hec_zflush.c
src/Hec_zwritxBytes.c
src/Hec_zarrayRetrieve.c
src/Hec_zsqueeze.c
src/Hec_zcheck.c
src/Hec_ztsRetrieve.c
src/Hec_zwhatChangedCompare.c
src/Hec_zopen.c
src/Hec_flushLogFile.c
src/Hec_closeLogFile.c
src/Hec_zset.c
src/Hec_zquery.c
src/Hec_zrenam.c
src/Hec_zget.c
src/Hec_zarrayStore.c
src/Hec_zgetRecordBasics.c
src/jni_utility.c
src/Hec_zgtrec.c
src/Hec_zcklnk.c
src/Hec_ztinRetrieve.c
src/Hec_zaliasList.c
src/Hec_zcofil.c
src/Hec_ztinStore.c
src/Hec_zclose.c
src/Hec_zinqir.c
src/Hec_zgetCatalogSortAddresses.c
src/Hec_zdelet.c
src/Hec_zgetDssVersion.c
src/Hec_zlocationStore.c
src/Hec_zcheckFile.c
src/Hec_zcatalogFile.c
src/Hec_zgetMessageLevel.c
src/Hec_zlocationRetrieve.c
src/Hec_writeToLogFile.c
src/Hec_zgetFileVersion.c
src/Hec_zgridStore.c
src/Hec_zsetMessageLevel.c
src/Hec_zlastWriteTimeFile.c
src/Hec_zreadxBytes.c
src/Hec_zerror.c
src/Hec_zlastWriteTime.c
src/Hec_zwhatChanged.c
src/Hec_zlocationToStruct.c
src/JavaCatalogToC.c
src/Hec_zgridRetrieve.c
src/Hec_ztsends.c
src/Hec_zlocationFromStruct.c
src/javaHeclib.h
src/Hec_zmaxpart.c
src/Hec_ztsStore.c
src/Hec_zsetCatalogSortAddresses.c
src/Hec_ztextRetrieve.c
src/Hec_zmessage.c
src/Hec_zaliasGetPrimary.c
src/Hec_zinquire.c
src/Hec_zckpnb.c
src/Hec_zsetfi.c
 )
else()
    message(STATUS "No JNI JDK Found. Not building Java Heclib.")
endif()