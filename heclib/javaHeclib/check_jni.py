# used to compare C JNI calls with Java native method declarations
# useful when removing JNI calls from C code
import os
import re

def get_java_calls(java_file_path):
    """
    Return a list of native method names declared in the given Java source file.
    Only methods declared with the 'native' keyword (JNI) are returned.
    """
    with open(java_file_path, 'r', encoding='utf-8') as f:
        code = f.read()

    # Remove comments while preserving string/char literals and text blocks
    comment_pattern = re.compile(
        r'("(?:(?:\\.)|[^"\\])*")'        # double-quoted string
        r"|('(?:(?:\\.)|[^'\\])*')"       # single-quoted char literal
        r'|("""[\s\S]*?""")'              # Java text block ("""...""")
        r'|(//[^\n]*$)'                   # line comment
        r'|(/\*[\s\S]*?\*/)',             # block comment
        re.MULTILINE
    )

    def _comment_replacer(m):
        # If it's a comment (groups 4 or 5), drop it; otherwise keep the literal as-is
        if m.group(4) or m.group(5):
            return ''
        return m.group(0)

    code = comment_pattern.sub(_comment_replacer, code)

    # Remove annotations (including those with parameters) to simplify parsing
    # e.g., @Override, @Size(max=5), @com.pkg.Ann(x=1)
    code = re.sub(r'@\w+(?:\.\w+)*(?:\s*\([^)]*\))?', '', code)

    # Regex to find native method declarations (no body, ends with ';')
    # Handles modifiers in any order, optional type params, return types with generics/arrays, optional throws.
    native_method_pattern = re.compile(
        r'(?:(?:^|\s))'                                   # start or whitespace
        r'(?:(?:public|protected|private|static|final|abstract|synchronized|strictfp|transient)\s+)*'
        r'native\s+'                                      # 'native' must be present
        r'(?:<[^>;{}()]*>\s+)?'                           # optional type parameters
        r'(?:[\w\[\].<>?]+\s+)+'                          # return type (tokens)
        r'(?P<name>[A-Za-z_]\w*)\s*'                      # method name
        r'\('
        r'[^)]*'                                          # params (annotations removed above)
        r'\)\s*'
        r'(?:throws\s+[\w\.,\s<>?]+\s*)?'                 # optional throws
        r';',                                             # no body: ends with semicolon
        re.MULTILINE | re.DOTALL
    )

    return [m.group('name') for m in native_method_pattern.finditer(code)]


def get_jni_calls(dir: str):

    pat = re.compile(
        r'JNIEXPORT\s+(?P<ret>\w+)\s+JNICALL\s+Java_hec_heclib_util_Heclib_(?P<fn>\w+)\s*\('
    )
    results = []
    for root, _, files in os.walk(dir):
        for file in files:
            if file.endswith('.c'):
                path = os.path.join(root, file)
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                for m in pat.finditer(content):
                    results.append({
                        'file': path,
                        'match': m.group(0),      # full matched text
                        'ret': m.group('ret'),
                        'fn': m.group('fn'),
                    })
    return results

c_calls = get_jni_calls('src')
c_functions = {c['fn'].replace("Hec_1","Hec_") for c in c_calls}
c_functions = sorted(c_functions)

print("found {} JNI calls in C code:".format(len(c_calls)))
c_functions.sort()
print(c_functions)
java_calls = get_java_calls(r"C:\project\hec-monolith\hec-monolith\src\main\java\hec\heclib\util\Heclib.java")
java_calls.sort()
print("found {} JNI calls in Java code:".format(len(java_calls)))
print(java_calls)

missing = set(c_functions) - set(java_calls)
print("missing in Java code:")
print(missing)
missing = set(java_calls) - set(c_functions)
print("Missing in C code:")
missing = set(java_calls) - set(c_functions)
print(missing)