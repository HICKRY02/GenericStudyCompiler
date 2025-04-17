#include <concepts>
#include <istream>
#include <ostream>

template<typename T>
class Serializer {
public:
    static void Serialize(const T& obj, std::ostream& os) {
        static_assert(sizeof(T) == 0, "Serialize not implemented for this type.");
    }

    static T Deserialize(std::istream& is) {
        static_assert(sizeof(T) == 0, "Deserialize not implemented for this type.");
    }
};

template<typename T>
concept Serializable = requires(T& obj, std::istream& is, std::ostream& os) {
    { Serializer<T>::Serialize(obj, os) };
    { Serializer<T>::Deserialize(is) } -> std::same_as<T>;
};