import sys

def file_to_header(input_file, output_file, array_name):
    with open(input_file, 'rb') as f:
        data = f.read()
    
    with open(output_file, 'w') as f:
        f.write(f'const unsigned char {array_name}[] = {{\n')
        for i, byte in enumerate(data):
            f.write(f'0x{byte:02x}, ')
            if (i + 1) % 16 == 0:
                f.write('\n')
        f.write('};\n')
        f.write(f'const unsigned int {array_name}_len = {len(data)};\n')

if __name__ == '__main__':
    file_to_header('TimeWarpIDE.exe', 'payload.h', 'timewarp_exe')
