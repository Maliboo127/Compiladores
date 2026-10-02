#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>

using namespace std;

// ============================================================
// 1. TOKENS
// ============================================================
enum class Tipo { MNEMONICO, REGISTRO, COMA, NUMERO, FIN_ENTRADA };

struct Token {
    Tipo tipo;
    string lexema;
};

static string nombreToken(const Token& t) {
    switch (t.tipo) {
        case Tipo::MNEMONICO: return t.lexema;  // MOV, ADD, STO, END
        case Tipo::REGISTRO:  return "REGISTRO";
        case Tipo::COMA:      return "COMA";
        case Tipo::NUMERO:    return "NUMERO";
        default:              return "FIN";
    }
}

// Excepción para errores de validación
struct ErrorInstruccion : runtime_error {
    using runtime_error::runtime_error;
};

// ============================================================
// 2. AFD LÉXICO (carácter por carácter)
//    Estados: L_INICIO, L_PALABRA (letras), L_NUMERO (dígitos)
//    Clases de carácter: LETRA, DIGITO, ESPACIO, COMA, OTRO, EOF
// ============================================================
enum class EstadoLex { L_INICIO, L_PALABRA, L_NUMERO };
enum class Clase { LETRA, DIGITO, ESPACIO, COMA, OTRO, FIN };

static Clase clasificar(char c, bool fin) {
    if (fin) return Clase::FIN;
    if (c >= 'A' && c <= 'Z') return Clase::LETRA;   // solo mayúsculas
    if (c >= '0' && c <= '9') return Clase::DIGITO;
    if (c == ' ' || c == '\t') return Clase::ESPACIO;
    if (c == ',') return Clase::COMA;
    return Clase::OTRO;
}

// Clasifica una palabra completa (mnemónico o registro)
static Token clasificarPalabra(const string& w) {
    if (w == "MOV" || w == "ADD" || w == "STO" || w == "END")
        return {Tipo::MNEMONICO, w};
    if (w == "AL" || w == "BL")
        return {Tipo::REGISTRO, w};
    // Palabra de 2 letras terminada en L/X tipo registro (AX, CL...) -> registro inválido
    if (w.size() == 2 && (w[1] == 'L' || w[1] == 'X'))
        throw ErrorInstruccion("registro invalido '" + w + "' (solo se admiten AL y BL).");
    throw ErrorInstruccion("mnemonico o palabra no reconocida: '" + w + "'.");
}

static vector<Token> analizadorLexico(const string& entrada) {
    vector<Token> tokens;
    EstadoLex estado = EstadoLex::L_INICIO;
    string buffer;

    // Se procesa un carácter extra (fin de entrada) para cerrar el último lexema
    for (size_t i = 0; i <= entrada.size(); ++i) {
        bool fin = (i == entrada.size());
        char c = fin ? '\0' : entrada[i];
        Clase k = clasificar(c, fin);

        switch (estado) {
            case EstadoLex::L_INICIO:
                if (k == Clase::LETRA)      { buffer = string(1, c); estado = EstadoLex::L_PALABRA; }
                else if (k == Clase::DIGITO){ buffer = string(1, c); estado = EstadoLex::L_NUMERO; }
                else if (k == Clase::ESPACIO) { /* permanece */ }
                else if (k == Clase::COMA)  { tokens.push_back({Tipo::COMA, ","}); }
                else if (k == Clase::FIN)   { /* fin */ }
                else throw ErrorInstruccion(string("caracter no permitido '") + c +
                                            "' (mnemonicos y registros van en mayusculas).");
                break;

            case EstadoLex::L_PALABRA:
                if (k == Clase::LETRA) buffer += c;
                else if (k == Clase::DIGITO)
                    throw ErrorInstruccion("digito inesperado dentro de la palabra '" + buffer + c + "'.");
                else if (k == Clase::OTRO)
                    throw ErrorInstruccion(string("caracter no permitido '") + c + "'.");
                else {  // ESPACIO, COMA o FIN: cierra el lexema
                    tokens.push_back(clasificarPalabra(buffer));
                    buffer.clear();
                    estado = EstadoLex::L_INICIO;
                    if (k == Clase::COMA) tokens.push_back({Tipo::COMA, ","});
                }
                break;

            case EstadoLex::L_NUMERO:
                if (k == Clase::DIGITO) buffer += c;
                else if (k == Clase::LETRA)
                    throw ErrorInstruccion("letra inesperada dentro del numero '" + buffer + c + "'.");
                else if (k == Clase::OTRO)
                    throw ErrorInstruccion(string("caracter no permitido '") + c + "'.");
                else {
                    tokens.push_back({Tipo::NUMERO, buffer});
                    buffer.clear();
                    estado = EstadoLex::L_INICIO;
                    if (k == Clase::COMA) tokens.push_back({Tipo::COMA, ","});
                }
                break;
        }
    }
    tokens.push_back({Tipo::FIN_ENTRADA, ""});
    return tokens;
}

// ============================================================
// 3. OBJETO INSTRUCCIÓN
// ============================================================
struct Instruccion {
    string operacion;
    vector<string> registros;
    bool tieneDireccion = false;
    unsigned long long direccion = 0;

    string aTexto() const {
        string s = "{\n  operacion: \"" + operacion + "\",\n  registros: [";
        for (size_t i = 0; i < registros.size(); ++i)
            s += (i ? ", " : "") + string("\"") + registros[i] + "\"";
        s += "],\n  direccion: ";
        s += tieneDireccion ? to_string(direccion) : "null";
        s += "\n}";
        return s;
    }
};

// ============================================================
// 4. AFD SINTÁCTICO (sobre tokens): valida orden y componentes
//    Estados con nombre; el estado P_FIN es el único de aceptación.
// ============================================================
enum class EstadoSint {
    P0,
    MOV_REG, MOV_COMA, MOV_DIR,   // MOV R , d
    ADD_AL,  ADD_COMA, ADD_BL,    // ADD AL , BL
    STO_DIR,                      // STO d
    P_FIN                         // aceptación
};

static Instruccion analizadorSintactico(const vector<Token>& toks) {
    EstadoSint e = EstadoSint::P0;
    Instruccion ins;

    for (const Token& t : toks) {
        bool fin = (t.tipo == Tipo::FIN_ENTRADA);
        switch (e) {
            case EstadoSint::P0:
                if (fin) throw ErrorInstruccion("entrada vacia.");
                if (t.tipo != Tipo::MNEMONICO)
                    throw ErrorInstruccion("la instruccion debe iniciar con un mnemonico (MOV, ADD, STO o END).");
                ins.operacion = t.lexema;
                if (t.lexema == "MOV") e = EstadoSint::MOV_REG;
                else if (t.lexema == "ADD") e = EstadoSint::ADD_AL;
                else if (t.lexema == "STO") e = EstadoSint::STO_DIR;
                else e = EstadoSint::P_FIN;  // END
                break;

            // ---- MOV R, d ----
            case EstadoSint::MOV_REG:
                if (fin) throw ErrorInstruccion("falta el registro destino (AL o BL).");
                if (t.tipo != Tipo::REGISTRO)
                    throw ErrorInstruccion("se esperaba un registro (AL o BL) despues de MOV.");
                ins.registros.push_back(t.lexema);
                e = EstadoSint::MOV_COMA;
                break;
            case EstadoSint::MOV_COMA:
                if (t.tipo != Tipo::COMA)
                    throw ErrorInstruccion("falta la coma entre el registro y la direccion.");
                e = EstadoSint::MOV_DIR;
                break;
            case EstadoSint::MOV_DIR:
                if (t.tipo != Tipo::NUMERO)
                    throw ErrorInstruccion("falta la direccion (numero decimal) despues de la coma.");
                ins.tieneDireccion = true;
                try { ins.direccion = stoull(t.lexema); }
                catch (...) { throw ErrorInstruccion("la direccion '" + t.lexema + "' es demasiado grande."); }
                e = EstadoSint::P_FIN;
                break;

            // ---- ADD AL, BL ----
            case EstadoSint::ADD_AL:
                if (fin) throw ErrorInstruccion("faltan los operandos AL, BL.");
                if (t.tipo != Tipo::REGISTRO || t.lexema != "AL")
                    throw ErrorInstruccion("el primer operando de ADD debe ser AL.");
                ins.registros.push_back("AL");
                e = EstadoSint::ADD_COMA;
                break;
            case EstadoSint::ADD_COMA:
                if (t.tipo != Tipo::COMA)
                    throw ErrorInstruccion("falta la coma entre AL y BL.");
                e = EstadoSint::ADD_BL;
                break;
            case EstadoSint::ADD_BL:
                if (fin) throw ErrorInstruccion("falta el segundo operando (BL) después de la coma.");
                if (t.tipo != Tipo::REGISTRO || t.lexema != "BL")
                    throw ErrorInstruccion("el segundo operando de ADD debe ser BL.");
                ins.registros.push_back("BL");
                e = EstadoSint::P_FIN;
                break;

            // ---- STO d ----
            case EstadoSint::STO_DIR:
                if (fin) throw ErrorInstruccion("falta la direccion de memoria en STO.");
                if (t.tipo != Tipo::NUMERO)
                    throw ErrorInstruccion("STO requiere una direccion numerica.");
                ins.tieneDireccion = true;
                try { ins.direccion = stoull(t.lexema); }
                catch (...) { throw ErrorInstruccion("la direccion '" + t.lexema + "' es demasiado grande."); }
                e = EstadoSint::P_FIN;
                break;

            // ---- Aceptación: solo se admite el fin de la entrada ----
            case EstadoSint::P_FIN:
                if (!fin)
                    throw ErrorInstruccion("operandos sobrantes después de la instrucción ('" + t.lexema + "').");
                break;
        }
    }

    if (e != EstadoSint::P_FIN) throw ErrorInstruccion("instruccion incompleta.");
    return ins;
}

// ============================================================
// 5. MÁQUINA DE MOORE
//    La salida depende únicamente del estado actual.
//    Recibe el objeto instrucción; no vuelve a analizar el texto.
// ============================================================
enum class EstadoMoore {
    PREPARAR_DIRECCION,  // MAR <- direccion
    LEER_MEMORIA,        // MBR <- M[MAR]
    CARGAR_REGISTRO,     // R <- MBR
    SUMAR,               // ACC <- AL + BL
    LEER_ACC,            // MBR <- ACC
    ESCRIBIR_MEMORIA,    // M[MAR] <- MBR
    DETENER,             // HALT <- 1
    FIN                  // sin salida
};

class MaquinaMoore {
    const Instruccion& ins;
    EstadoMoore estado;

    // Función de salida (λ): estado -> microoperación
    string salida(EstadoMoore s) const {
        switch (s) {
            case EstadoMoore::PREPARAR_DIRECCION: return "MAR ← " + to_string(ins.direccion);
            case EstadoMoore::LEER_MEMORIA:       return "MBR ← M[MAR]";
            case EstadoMoore::CARGAR_REGISTRO:    return ins.registros[0] + " ← MBR";
            case EstadoMoore::SUMAR:              return "ACC ← AL + BL";
            case EstadoMoore::LEER_ACC:           return "MBR ← ACC";
            case EstadoMoore::ESCRIBIR_MEMORIA:   return "M[MAR] ← MBR";
            case EstadoMoore::DETENER:            return "HALT ← 1";
            default:                              return "";
        }
    }

    // Función de transición (δ): (estado, operación) -> siguiente estado
    EstadoMoore siguiente(EstadoMoore s) const {
        switch (s) {
            case EstadoMoore::PREPARAR_DIRECCION:
                return ins.operacion == "MOV" ? EstadoMoore::LEER_MEMORIA
                                              : EstadoMoore::LEER_ACC;   // STO
            case EstadoMoore::LEER_MEMORIA:     return EstadoMoore::CARGAR_REGISTRO;
            case EstadoMoore::CARGAR_REGISTRO:  return EstadoMoore::FIN;
            case EstadoMoore::SUMAR:            return EstadoMoore::FIN;
            case EstadoMoore::LEER_ACC:         return EstadoMoore::ESCRIBIR_MEMORIA;
            case EstadoMoore::ESCRIBIR_MEMORIA: return EstadoMoore::FIN;
            case EstadoMoore::DETENER:          return EstadoMoore::FIN;
            default:                            return EstadoMoore::FIN;
        }
    }

public:
    explicit MaquinaMoore(const Instruccion& i) : ins(i) {
        // Estado inicial según la ruta de la instrucción
        if (ins.operacion == "MOV" || ins.operacion == "STO") estado = EstadoMoore::PREPARAR_DIRECCION;
        else if (ins.operacion == "ADD")                      estado = EstadoMoore::SUMAR;
        else                                                  estado = EstadoMoore::DETENER;  // END
    }

    bool terminada() const { return estado == EstadoMoore::FIN; }

    // Emite la salida del estado actual y avanza al siguiente
    string siguientePaso() {
        string out = salida(estado);
        estado = siguiente(estado);
        return out;
    }
};

// ============================================================
// 6. PROGRAMA PRINCIPAL
// ============================================================
static void procesar(const string& entrada) {
    cout << "ENTRADA\n" << entrada << "\n\nVALIDACION MEDIANTE AFD\n";

    vector<Token> tokens;
    Instruccion ins;
    try {
        tokens = analizadorLexico(entrada);
        ins = analizadorSintactico(tokens);
    } catch (const ErrorInstruccion& e) {
        cout << "Instruccion invalida: " << e.what() << "\n"
             << "No se construye el objeto instruccion.\n"
             << "No se generan microoperaciones.\n";
        return;
    }
    cout << "Instruccion valida.\n";

    cout << "\nTOKENS RECONOCIDOS\n";
    for (const Token& t : tokens)
        if (t.tipo != Tipo::FIN_ENTRADA)
            cout << nombreToken(t) << "(\"" << t.lexema << "\")\n";

    cout << "\nCOMPONENTES IDENTIFICADOS\n";
    cout << "Mnemonico: " << ins.operacion << "\n";
    if (ins.operacion == "MOV") {
        cout << "Registro destino: " << ins.registros[0] << "\n";
    } else if (ins.operacion == "ADD") {
        cout << "Registros: " << ins.registros[0] << ", " << ins.registros[1] << "\n";
    }
    if (ins.tieneDireccion) {
        cout << "Direccion de memoria: " << ins.direccion << "\n";
        cout << "Direccionamiento: directo\n";
    }

    cout << "\nOBJETO INSTRUCCION\n" << ins.aTexto() << "\n";

    cout << "\nMICROOPERACIONES GENERADAS POR MOORE\n";
    MaquinaMoore moore(ins);
    int n = 1;
    while (!moore.terminada())
        cout << n++ << ". " << moore.siguientePaso() << "\n";
    cout << "Generacion terminada.\n";
}

int main(int argc, char* argv[]) {
    if (argc == 2 && string(argv[1]) == "--pruebas") {
        const vector<string> pruebas = {
            "MOV AL, 6", "MOV BL, 7", "ADD AL, BL", "STO 8", "END",
            "MOV AX, 6", "MOV AL 6", "ADD AL,", "STO", "END 8"
        };
        for (const string& p : pruebas) {
            procesar(p);
            cout << "\n----------------------------------------\n\n";
        }
        return 0;
    }

    string entrada;
    if (argc > 1) {
        for (int i = 1; i < argc; ++i) entrada += (i > 1 ? " " : "") + string(argv[i]);
    } else {
        cout << "Instruccion: ";
        getline(cin, entrada);
    }
    procesar(entrada);
    return 0;
}
