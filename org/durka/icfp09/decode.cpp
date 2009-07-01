#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>
using namespace std;

union instr
{
    unsigned int raw;
    struct
    {
        unsigned R2     :14;
        unsigned R1     :14;
        unsigned OP     :4;
    } D;
    struct
    {
        unsigned R1     :14;
        unsigned junk   :7;
        unsigned IMM    :3;
        unsigned OP     :4;
        unsigned zero   :4; // if this is zero, it is S-type, otherwise D-type
    } S;
};

struct frame
{
    int i;
    union
    {
        char raw[12];
        struct
        {
            instr code;
            double data;
        } odd;
        struct
        {
            double data;
            instr code;
        } even;
    };
};

int read_file(char*, char**);
void decode(char*, frame**, int);
string summarize(instr);

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        cout << "Usage: decode <filename>";
        return 1;
    }

    char *file;
    int len = read_file(argv[1], &file);
    if (len == -1)
    {
        cerr << "File could not be read.\n";
        return 1;
    }
    len /= 12;
    
    frame *decoded;
    decode(file, &decoded, len);

    ostringstream summaries;
    cout << setprecision(1000);
    for (int i = 0; i < len; ++i)
    {
        cout << (decoded[i].i % 2 ? decoded[i].odd.data : decoded[i].even.data) << " ";
        summaries << summarize(decoded[i].i % 2 ? decoded[i].odd.code : decoded[i].even.code) << endl;
    }
    cout << endl << summaries.str();

    delete[] decoded;
    delete[] file;
    return 0;
}

int read_file(char *filename, char **out)
{
    ifstream file(filename, ios::in | ios::binary | ios::ate);
    if (file.is_open())
    {
        ifstream::pos_type size = file.tellg();
        *out = new char[size];

        file.seekg(0, ios::beg);
        file.read(*out, size);
        file.close();

        return size;
    }
    return -1;
}

void decode(char *in, frame **out, int len)
{
    *out = new frame[len];
    for (int frm = 0; frm < len; ++frm)
    {
        (*out)[frm].i = frm;
        memcpy(&((*out)[frm].raw), in + (frm * 12), 12);
    }
}

string summarize(instr code)
{
    ostringstream out;

    if (code.S.zero)
    {
        switch (code.D.OP)
        {
            case 1:
                out << "Add ";
                break;
            case 2:
                out << "Sub ";
                break;
            case 3:
                out << "Mult ";
                break;
            case 4:
                out << "Div ";
                break;
            case 5:
                out << "Output ";
                break;
            case 6:
                out << "Phi ";
                break;
            default:
                out << "UNRECOGNIZED ";
        }
        out << hex << uppercase << "0x" << code.D.R1 << " 0x" << code.D.R2;
    }
    else
    {
        switch (code.S.OP)
        {
            case 0:
                out << "Noop ";
                break;
            case 1:
                out << "Cmpz " << (string[]){"<", "<=", "=", ">=", ">"}[code.S.IMM] << hex << uppercase << " 0x" << code.S.R1;
                break;
            case 2:
                out << "Sqrt 0x" << hex << uppercase << code.S.R1;
                break;
            case 3:
                out << "Copy 0x" << hex << uppercase << code.S.R1;
                break;
            case 4:
                out << "Input 0x" << hex << uppercase << code.S.R1;
                break;
            default:
                out << "UNRECOGNIZED";
        }
    }

    return out.str();
}
