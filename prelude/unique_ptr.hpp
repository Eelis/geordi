#ifndef UNIQUE_PTR_HPP_
#define UNIQUE_PTR_HPP_

#include <type_traits>
#include <utility>
#include <cassert>

template <class T> struct default_delete;
template <class T> struct default_delete<T[]>;
template <class T, size_t N> struct default_delete<T[N]>;

template <class T, class D = default_delete<T>> class unique_ptr;
template <class T, class D> class unique_ptr<T[], D>;
template <class T, class D, size_t N> class unique_ptr<T[N], D>;

template <class T> struct default_delete {
    default_delete() {}
    template <class U> default_delete(const default_delete<U>&) {}
    void operator()(T * ptr) const {
        static_assert(sizeof(T) > 0, "Can't delete pointer to incomplete type");
        delete ptr;
    }
};

template <class T> struct default_delete<T[]> {
    void operator()(T * ptr) const {
        static_assert(sizeof(T) > 0, "Can't delete pointer to incomplete type");
        delete [] ptr;
    }
};

template <class T, size_t N> struct default_delete<T[N]> {
    void operator()(T * ptr, size_t) const {
        static_assert(sizeof(T) > 0, "Can't delete pointer to incomplete type");
        delete [] ptr;
    }
};

template <class T, class D> class unique_ptr {
    typedef unique_ptr<T,D> this_type;
    typedef T * this_type::* unspecified_bool_type;
    typedef T * this_type::* unspecified_pointer_type;
public:
    typedef T element_type;
    typedef T* pointer;
    typedef D deleter_type;
    
    // constructors
    unique_ptr() : p_(0) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    explicit unique_ptr(T* p) : p_(p) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    
    unique_ptr(T* p, typename std::conditional<std::is_reference<D>::value, D, const D&>::type d) : p_(p), d_(d) {}
    unique_ptr(T* p, typename std::remove_reference<D>::type && d) : p_(p), d_(std::move(d)) {
        static_assert(!std::is_reference<D>::value, "rvalue deleter bound to reference");
    }
    
    // move constructors
    unique_ptr(unique_ptr && u) : p_(u.release()), d_(std::forward<D>(u.get_deleter())) {}
    template <class U, class E> unique_ptr(unique_ptr<U, E> && u) : p_(u.release()), d_(std::forward<D>(u.get_deleter())) {}
    //unique_ptr(unique_ptr && u) : p_(u.release()), d_(u.get_deleter()) {}
    //template <class U, class E> unique_ptr(unique_ptr<U, E> && u) : p_(u.release()), d_(u.get_deleter()) {}
    
    // destructor
    ~unique_ptr() { d_(p_); }
    
    // assignment
    unique_ptr& operator=(unique_ptr&& u) {
        reset(u.release()); d_ = std::move(u.d_); 
        /*this_type( u ).swap( *this );*/ return *this;
    }
    template <class U, class E> unique_ptr& operator=(unique_ptr<U, E>&& u) {
        reset(u.release()); d_ = std::move(u.get_deleter()); 
        /*this_type( u ).swap( *this );*/ return *this;
    }
    unique_ptr& operator=(unspecified_pointer_type) { reset(); return *this;}
    
    // observers
    typename std::add_lvalue_reference<T>::type operator*() const { assert(p_ != 0); return *p_; } 
    T* operator->() const { assert(p_ != 0); return p_; }
    T* get() const { return p_; }
    bool operator! () const { return p_ == 0; }
    D& get_deleter() { return d_; }
    const D& get_deleter() const { return d_; }    
    operator unspecified_bool_type () const { return p_ == 0 ? 0 : &this_type::p_; }
    
    // modifiers
    T* release() { T* t = p_; p_ = 0; return t; }
    void reset(T* p = 0) { assert(p == 0 || p != p_); d_(p_); p_ = p; }
    
    void swap(unique_ptr && u) {
        T * t(std::move(u.p_));
        u.p_ = std::move(p_);
        p_ = std::move(t);
        
        D d(std::move(u.d_));
        u.d_ = std::move(d_);
        d_ = std::move(d);
    }
private:    
    T * p_;
    D d_;
    
    // disable copy from lvalue
    unique_ptr(const unique_ptr&);
    template <class U, class E> unique_ptr(const unique_ptr<U, E>&);
    unique_ptr& operator=(const unique_ptr&);
    template <class U, class E> unique_ptr& operator=(const unique_ptr<U, E>&);
};

template <class T, class D> class unique_ptr<T[], D> {
    typedef unique_ptr<T,D> this_type;
    typedef T * this_type::* unspecified_bool_type;
    typedef T * this_type::* unspecified_pointer_type;
public:
    typedef T element_type;
    typedef T* pointer;
    typedef D deleter_type;
    
    // constructors
    unique_ptr() : p_(0) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    explicit unique_ptr(T* p) : p_(p) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    
    unique_ptr(T* p, typename std::conditional<std::is_reference<D>::value, D, const D&>::type d) : p_(p), d_(d) {}
    unique_ptr(T* p, typename std::remove_reference<D>::type && d) : p_(p), d_(std::move(d)) {
        static_assert(!std::is_reference<D>::value, "rvalue deleter bound to reference");
    }

    unique_ptr(unique_ptr&& u) : p_(u.release()), d_(std::forward<D>(u.d_)) {}
    
    // destructor
    ~unique_ptr() { d_(p_); }
    
    // assignment
    unique_ptr& operator=(unique_ptr&& u) {reset(u.release()); d_ = std::move(u.d_); return *this;}
    unique_ptr& operator=(unspecified_pointer_type) { reset(); return *this;}
    
    // observers
    typename std::add_lvalue_reference<T>::type operator[](size_t i) const { return p_[i]; }
    T* get() const { return p_; }
    D& get_deleter() { return d_; }
    const D& get_deleter() const { return d_; }
    operator unspecified_bool_type () const { return p_ == 0 ? 0 : &this_type::p_; }
    
    // modifiers
    T* release() { T* t = p_; p_ = 0; return t; }
    void reset(T* p = 0) { assert(p == 0 || p != p_); d_(p_); p_ = p; }
    
    void swap(unique_ptr && u) {
        T * t(std::move(u.p_));
        u.p_ = std::move(p_);
        p_ = std::move(t);
        
        D d(std::move(u.d_));
        u.d_ = std::move(d_);
        d_ = std::move(d);
    }
private:
    T * p_;
    D d_;
    
    // disable copy from lvalue
    unique_ptr(const unique_ptr&);
    unique_ptr& operator=(const unique_ptr&);
};

template <class T, class D, size_t N> class unique_ptr<T[N], D> {
    typedef unique_ptr<T,D> this_type;
    typedef T * this_type::* unspecified_bool_type;
    typedef T * this_type::* unspecified_pointer_type;
public:
    typedef T element_type;
    typedef T* pointer;
    typedef D deleter_type;
    static const size_t size = N;
    
    // constructors
    unique_ptr() : p_(0) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    explicit unique_ptr(T* p) : p_(p) {static_assert(!std::is_pointer<D>::value, "Constructed with null function pointer deleter");}
    
    unique_ptr(T* p, typename std::conditional<std::is_reference<D>::value, D, const D&>::type d) : p_(p), d_(d) {}
    unique_ptr(T* p, typename std::remove_reference<D>::type && d) : p_(p), d_(std::move(d)) {
        static_assert(!std::is_reference<D>::value, "rvalue deleter bound to reference");
    }
    
    // move constructor
    unique_ptr(unique_ptr&& u) : p_(u.release()), d_(std::forward<D>(u.d_)) {}
    
    // destructor
    ~unique_ptr() { d_(p_, N); }
    
    // assignment
    unique_ptr& operator=(unique_ptr&& u) {reset(u.release()); d_ = std::move(u.d_); return *this;}
    unique_ptr& operator=(unspecified_pointer_type) { reset(); return *this;}
    
    // observers
    typename std::add_lvalue_reference<T>::type operator[](size_t i) const { assert(i < N); return p_[i]; }
    T* get() const { return p_; }
    D& get_deleter() { return d_; }
    const D& get_deleter() const { return d_; }
    operator unspecified_bool_type () const { return p_ == 0 ? 0 : &this_type::p_; }
    
    // modifiers
    T* release() { T* t = p_; p_ = 0; return t; }
    void reset(T* p = 0) { assert(p == 0 || p != p_); d_(p_); p_ = p; }
    
    void swap(unique_ptr && u) {
        T * t(std::move(u.p_));
        u.p_ = std::move(p_);
        p_ = std::move(t);
        
        D d(std::move(u.d_));
        u.d_ = std::move(d_);
        d_ = std::move(d);
    }
private:
    T* p_;
    D d_;
    
    // disable copy from lvalue
    unique_ptr(const unique_ptr&);
    unique_ptr& operator=(const unique_ptr&);
};

template<class T, class D> 
inline void swap(unique_ptr<T, D>& x, unique_ptr<T, D>& y) {x.swap(y);}

template<class T, class D> 
inline void swap(unique_ptr<T, D>&& x, unique_ptr<T, D>& y){x.swap(y);}

template<class T, class D> 
inline void swap(unique_ptr<T, D>& x, unique_ptr<T, D>&& y){x.swap(y);}

template<class T1, class D1, class T2, class D2>
inline bool operator==(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() == y.get(); }

template<class T1, class D1, class T2, class D2>
inline bool operator!=(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() != y.get(); }

template<class T1, class D1, class T2, class D2>
inline bool operator<(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() < y.get(); }

template<class T1, class D1, class T2, class D2>
inline bool operator<=(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() <= y.get(); }

template<class T1, class D1, class T2, class D2>
inline bool operator>(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() > y.get(); }

template<class T1, class D1, class T2, class D2>
inline bool operator>=(const unique_ptr<T1, D1>& x, const unique_ptr<T2, D2>& y)
{ return x.get() >= y.get(); }

#endif

