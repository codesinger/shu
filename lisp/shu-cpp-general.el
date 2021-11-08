`;;; shu-cpp-general.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-general
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is free software: you can redistrcibute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; There is a copy of the Gnu General Public license in the file
;; LICENSE in this repository.  You should also have received a copy
;; of the GNU General Public License along with GNU Emacs.  If not,
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A collection of useful functions for dealing with C++ code.
;;
;; ## Selected highlights ##
;;
;; Here are some useful features of this package.
;;
;; ### Dealing with long string constants ###
;;
;; If you copy strings of text into string constants in your program, you may end up
;; with some very long lines.  SHU-CSPLIT can automatically split such a line
;; for you.  SHU-CUNSPLIT can undo the split.  SHU-CREPLACE can in one
;; operation, replace a split line with a different string constant.
;;

;;; Code:


(provide 'shu-cpp-general)
(require 'shu-base)
(require 'shu-cpp-token)


(defcustom shu-cpp-allocator-type "bslma::Allocator"
  "The data type that represents an allocator."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-date-type "bdlt::Date"
  "The data type that represents a date."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-datetime-type "bdlt::Datetime"
  "The data type that represents a date and time."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-datetime-timezone-type "bdlt::DatetimeTz"
  "The data type that represents a date and time with an associated time zone."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-interval-type "bdlt::DatetimeInterval"
  "The data type that represents a time interval type."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-short-interval-type "bsls::TimeInterval"
  "The data type that represents a short time interval type."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-long-long-type "bsls::Types::Int64"
  "The data type that represents a 64 bit integer."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-size-type "bsl::size_t"
  "The data type that represents a size."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-string-type "bsl::string"
  "The data type that represents a string type."
  :type '(string)
  :group 'shu-cpp-general)


(defcustom shu-cpp-time-type "bdlt::Time"
  "The data type that represents a time."
  :type '(string)
  :group 'shu-cpp-general)


(defconst shu-cpp-base-types
  (list
   "bool"
   "char"
   "double"
   "enum"
   "float"
   "int"
   "int16_t"
   "int32_t"
   "int64_t"
   "long"
   "short"
   "signed"
   "std::size_t"
   "uint16_t"
   "uint32_t"
   "uint64_t"
   "unsigned")
  "A list of all of the base types in C and C++.  This may be modified by shu-add-cpp-base-types")

(defvar shu-cpp-member-prefix "_"
  "The character string that is used as the prefix to member variables of a C++ class.
This is used by shu-internal-get-set when generating getters and setters for a class.")

(defvar shu-is-const nil
  "Set true if the C++ data member we are working is declared to be const.")

(defvar shu-nc-vtype nil
  "Set true if the C++ data member we are working is declared to be non-const.")

(defvar shu-var-name nil
  "The variable name that corresponds to an attribute name.")

(defvar shu-attr-name nil
  "The name of an attribute.")

(defvar shu-lc-comment nil
  "Comment string with the first letter downcased.")

(defvar shu-rmv-classes nil
  "An alist of \"using namespace\" directives and their line numbers where first declared.
Used to filter duplicates.")

(defvar shu-cpp-include-names nil
  "A hash table that maps class names to include file names  This is the hash table
inversion of shu-std-include-list or shu-bsl-include-list.")

;;
;;  shu-std-include-list
;;
(defconst shu-std-include-list
  (list
   (cons "algorithm"    (list
                         "std::all_of"
                         "std::any_of"
                         "std::none_of"
                         "std::for_each"
                         "std::for_each_n"
                         "std::count"
                         "std::count_if"
                         "std::mismatch"
                         "std::find"
                         "std::find_if"
                         "std::find_if_not"
                         "std::find_end"
                         "std::find_first_of"
                         "std::adjacent_find"
                         "std::search"
                         "std::search_n"
                         "std::copy"
                         "std::copy_if"
                         "std::copy_n"
                         "std::copy_backward"
                         "std::move"
                         "std::move_backward"
                         "std::fill"
                         "std::fill_n"
                         "std::transform"
                         "std::generate"
                         "std::generate_n"
                         "std::remove"
                         "std::remove_if"
                         "std::remove_copy"
                         "std::remove_copy_if"
                         "std::replace"
                         "std::replace_if"
                         "std::replace_copy"
                         "std::replace_copy_if"
                         "std::swap"
                         "std::swap_ranges"
                         "std::iter_swap"
                         "std::reverse"
                         "std::reverse_copy"
                         "std::rotate"
                         "std::rotate_copy"
                         "std::shift_left"
                         "std::shift_right"
                         "std::random_shuffle"
                         "std::shuffle"
                         "std::sample"
                         "std::unique"
                         "std::unique_copy"
                         "std::is_partitioned"
                         "std::partition"
                         "std::partition_copy"
                         "std::stable_partition"
                         "std::partition_point"
                         "std::is_sorted"
                         "std::is_sorted_until"
                         "std::sort"
                         "std::partial_sort"
                         "std::partial_sort_copy"
                         "std::stable_sort"
                         "std::nth_element"
                         "std::lower_bound"
                         "std::upper_bound"
                         "std::binary_search"
                         "std::equal_range"
                         "std::merge"
                         "std::inplace_merge"
                         "std::includes"
                         "std::set_difference"
                         "std::set_intersection"
                         "std::set_symmetric_difference"
                         "std::set_union"
                         "std::is_heap"
                         "std::is_heap_until"
                         "std::make_heap"
                         "std::push_heap"
                         "std::pop_heap"
                         "std::sort_heap"
                         "std::max"
                         "std::max_element"
                         "std::min"
                         "std::min_element"
                         "std::minmax"
                         "std::minmax_element"
                         "std::clamp"
                         "std::equal"
                         "std::lexicographical_compare"
                         "std::lexicographical_compare_three_way"
                         "std::is_permutation"
                         "std::next_permutation"
                         "std::prev_permutation"
                         ))
   (cons "atomic"    (list
                      "std::atomic"
                      "std::atomic_ref"
                      "std::atomic_flag"
                      "std::memory_order"
                      "std::atomic_bool"
                      "std::atomic_char"
                      "std::atomic_schar"
                      "std::atomic_uchar"
                      "std::atomic_short"
                      "std::atomic_ushort"
                      "std::atomic_int"
                      "std::atomic_uint"
                      "std::atomic_long"
                      "std::atomic_ulong"
                      "std::atomic_llong"
                      "std::atomic_ullong"
                      "std::atomic_char8_t"
                      "std::atomic_char16_t"
                      "std::atomic_char32_t"
                      "std::atomic_wchar_t"
                      "std::atomic_int8_t"
                      "std::atomic_uint8_t"
                      "std::atomic_int16_t"
                      "std::atomic_uint16_t"
                      "std::atomic_int32_t"
                      "std::atomic_uint32_t"
                      "std::atomic_int64_t"
                      "std::atomic_uint64_t"
                      "std::atomic_int_least8_t"
                      "std::atomic_uint_least8_t"
                      "std::atomic_int_least16_t"
                      "std::atomic_uint_least16_t"
                      "std::atomic_int_least32_t"
                      "std::atomic_uint_least32_t"
                      "std::atomic_int_least64_t"
                      "std::atomic_uint_least64_t"
                      "std::atomic_int_fast8_t"
                      "std::atomic_uint_fast8_t"
                      "std::atomic_int_fast16_t"
                      "std::atomic_uint_fast16_t"
                      "std::atomic_int_fast32_t"
                      "std::atomic_uint_fast32_t"
                      "std::atomic_int_fast64_t"
                      "std::atomic_uint_fast64_t"
                      "std::atomic_intptr_t"
                      "std::atomic_uintptr_t"
                      "std::atomic_size_t"
                      "std::atomic_ptrdiff_t"
                      "std::atomic_intmax_t"
                      "std::atomic_uintmax_t"
                      "std::atomic_signed_lock_free"
                      "std::atomic_unsigned_lock_free"
                      "std::atomic_is_lock_free"
                      "std::atomic_store"
                      "std::atomic_store_explicit"
                      "std::atomic_load"
                      "std::atomic_load_explicit"
                      "std::atomic_exchange"
                      "std::atomic_exchange_explicit"
                      "std::atomic_compare_exchange_weak"
                      "std::atomic_compare_exchange_weak_explicit"
                      "std::atomic_compare_exchange_strong"
                      "std::atomic_compare_exchange_strong_explicit"
                      "std::atomic_fetch_add"
                      "std::atomic_fetch_add_explicit"
                      "std::atomic_fetch_sub"
                      "std::atomic_fetch_sub_explicit"
                      "std::atomic_fetch_and"
                      "std::atomic_fetch_and_explicit"
                      "std::atomic_fetch_or"
                      "std::atomic_fetch_or_explicit"
                      "std::atomic_fetch_xor"
                      "std::atomic_fetch_xor_explicit"
                      "std::atomic_wait"
                      "std::atomic_wait_explicit"
                      "std::atomic_notify_one"
                      "std::atomic_notify_all"
                      "std::atomic_flag_test"
                      "std::atomic_flag_test_explicit"
                      "std::atomic_flag_test_and_set"
                      "std::atomic_flag_test_and_set_explicit"
                      "std::atomic_flag_clear"
                      "std::atomic_flag_clear_explicit"
                      "std::atomic_flag_wait"
                      "std::atomic_flag_wait_explicit"
                      "std::atomic_flag_notify_one"
                      "std::atomic_flag_notify_all"
                      "std::atomic_init"
                      "std::kill_dependency"
                      "std::atomic_thread_fence"
                      "std::atomic_signal_fence"
                      ))
   (cons "bitset"    (list
                      "std::bitset"
                      ))
   (cons "chrono"    (list
                      "std::time_point"
                      "std::system_clock"
                      "std::steady_clock"
                      "std::high_resolution_clock"
                      "std::time_point_cast"
                      "std::is_am"
                      "std::is_pm"
                      "std::get_tzdb"
                      "std::get_tzdb_list"
                      "std::reload_tzdb"
                      "std::remote_version"
                      ))
   (cons "cstddef"    (list
                       "std::size_t"
                       "std::ptrdiff_t"
                       "std::max_align_t"
                       "std::nullptr_t"
                       ))
   (cons "future"    (list
                      "std::promise"
                      "std::packaged_task"
                      "std::future"
                      "std::shared_future"
                      "std::launch"
                      "std::future_status"
                      "std::future_error"
                      "std::future_errc"
                      "std::async"
                      "std::future_category"
                      ))
   (cons "functional"    (list
                          "std::function"
                          "std::mem_fn"
                          "std::bad_function_call"
                          "std::is_bind_expression"
                          "std::is_placeholder"
                          "std::reference_wrapper"
                          ))
   (cons "iomanip"    (list
                       "std::resetiosflags"
                       "std::setiosflags"
                       "std::setbase"
                       "std::setfill"
                       "std::setprecision"
                       "std::setw"
                       "std::get_money"
                       "std::put_money"
                       "std::get_time"
                       "std::put_time"
                       "std::quoted"
                       ))
   (cons "iostream"    (list
                        "std::cin"
                        "std::wcin"
                        "std::cout"
                        "std::wcout"
                        "std::cerr"
                        "std::wcerr"
                        "std::clog"
                        "std::wclog"
                        ))
   (cons "ios"    (list
                   "std::basic_ios"
                   "std::fpos"
                   "std::ios"
                   "std::ios_base"
                   "std::wios"
                   "std::io_errc"
                   "std::streamoff"
                   "std::streampos"
                   "std::streamsize"
                   "std::wstreampos"
                   "std::boolalpha"
                   "std::showbase"
                   "std::showpoint"
                   "std::showpos"
                   "std::skipws"
                   "std::unitbuf"
                   "std::uppercase"
                   "std::noboolalpha"
                   "std::noshowbase"
                   "std::noshowpoint"
                   "std::noshowpos"
                   "std::noskipws"
                   "std::nounitbuf"
                   "std::nouppercase"
                   "std::dec"
                   "std::hex"
                   "std::oct"
                   "std::fixed"
                   "std::scientific"
                   "std::internal"
                   "std::left"
                   "std::right"
                   ))
   (cons "istream"    (list
                       "std::basic_istream"
                       "std::istream"
                       "std::wistream"
                       "std::basic_iostream"
                       "std::iostream"
                       "std::wiostream"
                       "std::ws"
                       ))
   (cons "locale"    (list
                      "std::narrow"
                      "std::tolower"
                      "std::toupper"
                      "std::widen"
                      ))
   (cons "limits"    (list
                      "std::numeric_limits"
                      ))
   (cons "map"    (list
                   "std::map"
                   "std::multimap"
                   ))
   (cons "memory"    (list
                      "std::allocator"
                      "std::allocator_arg"
                      "std::allocator_arg_t"
                      "std::allocator_traits"
                      "std::auto_ptr"
                      "std::auto_ptr_ref"
                      "std::shared_ptr"
                      "std::weak_ptr"
                      "std::unique_ptr"
                      "std::default_delete"
                      "std::make_shared"
                      "std::allocate_shared"
                      "std::static_pointer_cast"
                      "std::dynamic_pointer_cast"
                      "std::const_pointer_cast"
                      "std::get_deleter"
                      "std::owner_less"
                      "std::enable_shared_from_this"
                      "std::raw_storage_iterator"
                      "std::get_temporary_buffer"
                      "std::return_temporary_buffer"
                      "std::uninitialized_copy"
                      "std::uninitialized_copy_n"
                      "std::uninitialized_fill"
                      "std::uninitialized_fill_n"
                      "std::uninitialized_move"
                      "std::uninitialized_move_n"
                      "std::uninitialized_default_construct"
                      "std::uninitialized_default_construct_n"
                      "std::uninitialized_value_construct"
                      "std::uninitialized_value_construct_n"
                      "std::destroy"
                      "std::destroy_at"
                      "std::destroy_n"
                      "std::make_unique"
                      "std::pointer_traits"
                      "std::pointer_safety"
                      "std::declare_reachable"
                      "std::undeclare_reachable"
                      "std::declare_no_pointers"
                      "std::undeclare_no_pointers"
                      "std::get_pointer_safety"
                      "std::align"
                      "std::addressof"
                      "std::reinterpret_pointer_cast"
                      ))
   (cons "new"   (list
                  "std::bad_alloc"
                  "std::bad_array_new_length"
                  "std::nothrow_t"
                  "std::align_val_t"
                  "std::new_handler"
                  "std::hardware_destructive_interference_size"
                  "std::hardware_constructive_interference_size"
                  "std::get_new_handler"
                  "std::set_new_handler"
                  "std::launder"
                  ))
   (cons "optional"   (list
                       "std::optional"
                       "std::bad_optional_access"
                       "std::nullopt_t"
                       "std::nullopt"
                       "std::make_optional"
                       ))
   (cons "ostream"   (list
                      "std::basic_ostream"
                      "std::ostream"
                      "std::wostream"
                      "std::endl"
                      "std::ends"
                      "std::flush"
                      "std::emit_on_flush"
                      "std::noemit_on_flush"
                      "std::flush_emit"
                      ))
   (cons "queue"   (list
                    "std::queue"
                    "std::priority_queue"
                    ))
   (cons "set"   (list
                  "std::set"
                  "std::multiset"
                  ))
   (cons "regex"   (list
                    "std::basic_regex"
                    "std::sub_match"
                    "std::match_results"
                    "std::regex_iterator"
                    "std::regex_token_iterator"
                    "std::regex_error"
                    "std::regex_traits"
                    "std::regex_match"
                    "std::regex_search"
                    "std::regex_replace"
                    ))
   (cons "sstream"   (list
                      "std::istringstream"
                      "std::ostringstream"
                      "std::stringbuf"
                      "std::stringstream"
                      "std::wistringstream"
                      "std::wostringstream"
                      "std::wstringbuf"
                      "std::wstringstream"
                      ))
   (cons "stdexcept"    (list
                         "std::domain_error"
                         "std::invalid_argument"
                         "std::length_error"
                         "std::logic_error"
                         "std::out_of_range"
                         "std::overflow_error"
                         "std::range_error"
                         "std::runtime_error"
                         "std::underflow_error"
                         ))
   (cons "string"   (list
                     "std::basic_string"
                     "std::char_traits"
                     "std::string"
                     "std::u16string"
                     "std::wstring"
                     "std::stoi"
                     "std::stol"
                     "std::stoul"
                     "std::stoll"
                     "std::stoull"
                     "std::stof"
                     "std::stod"
                     "std::stold"
                     "std::to_string"
                     "std::to_wstring"
                     ))
   (cons "string_view"   (list
                          "std::string_view"
                          "std::u8string_view"
                          "std::u16string_view"
                          "std::u32string_view"
                          "std::wstring_view"
                          ))
   (cons "tuple"    (list
                     "std::tuple"
                     "std::tuple_size"
                     "std::tuple_element"
                     "std::make_tuple"
                     "std::forward_as_tuple"
                     "std::tie"
                     "std::tuple_cat"
                     "std::get"
                     "std::ignore"
                     ))
   (cons "type_traits"    (list
                           "std::integral_constant"
                           "std::bool_constant"
                           "std::true_type"
                           "std::false_type"
                           "std::is_void"
                           "std::is_null_pointer"
                           "std::is_integral"
                           "std::is_floating_point"
                           "std::is_array"
                           "std::is_enum"
                           "std::is_union"
                           "std::is_class"
                           "std::is_function"
                           "std::is_pointer"
                           "std::is_lvalue_reference"
                           "std::is_rvalue_reference"
                           "std::is_member_object_pointer"
                           "std::is_member_function_pointer"
                           "std::is_fundamental"
                           "std::is_arithmetic"
                           "std::is_scalar"
                           "std::is_object"
                           "std::is_compound"
                           "std::is_reference"
                           "std::is_member_pointer"
                           "std::is_const"
                           "std::is_volatile"
                           "std::is_trivial"
                           "std::is_trivially_copyable"
                           "std::is_standard_layout"
                           "std::is_pod"
                           "std::has_unique_object_representations"
                           "std::is_empty"
                           "std::is_polymorphic"
                           "std::is_abstract"
                           "std::is_final"
                           "std::is_aggregate"
                           "std::is_signed"
                           "std::is_unsigned"
                           "std::is_bounded_array"
                           "std::is_unbounded_array"
                           "std::is_scoped_enum"
                           "std::is_constructible"
                           "std::is_trivially_constructible"
                           "std::is_nothrow_constructible"
                           "std::is_default_constructible"
                           "std::is_trivially_default_constructible"
                           "std::is_nothrow_default_constructible"
                           "std::is_copy_constructible"
                           "std::is_trivially_copy_constructible"
                           "std::is_nothrow_copy_constructible"
                           "std::is_move_constructible"
                           "std::is_trivially_move_constructible"
                           "std::is_nothrow_move_constructible"
                           "std::is_assignable"
                           "std::is_trivially_assignable"
                           "std::is_nothrow_assignable"
                           "std::is_copy_assignable"
                           "std::is_trivially_copy_assignable"
                           "std::is_nothrow_copy_assignable"
                           "std::is_move_assignable"
                           "std::is_trivially_move_assignable"
                           "std::is_nothrow_move_assignable"
                           "std::is_destructible"
                           "std::is_trivially_destructible"
                           "std::is_nothrow_destructible"
                           "std::has_virtual_destructor"
                           "std::is_swappable_with"
                           "std::is_swappable"
                           "std::is_nothrow_swappable_with"
                           "std::is_nothrow_swappable"
                           "std::alignment_of"
                           "std::rank"
                           "std::extent"
                           "std::is_same"
                           "std::is_base_of"
                           "std::is_convertible"
                           "std::is_nothrow_convertible"
                           "std::is_layout_compatible"
                           "std::is_pointer_interconvertible_base_of"
                           "std::is_invocable"
                           "std::is_invocable_r"
                           "std::is_nothrow_invocable"
                           "std::is_nothrow_invocable_r"
                           "std::remove_cv"
                           "std::remove_const"
                           "std::remove_volatile"
                           "std::add_cv"
                           "std::add_const"
                           "std::add_volatile"
                           "std::remove_reference"
                           "std::add_lvalue_reference"
                           "std::add_rvalue_reference"
                           "std::remove_pointer"
                           "std::add_pointer"
                           "std::make_signed"
                           "std::make_unsigned"
                           "std::remove_extent"
                           "std::remove_all_extents"
                           "std::aligned_storage"
                           "std::aligned_union"
                           "std::decay"
                           "std::remove_cvref"
                           "std::enable_if"
                           "std::conditional"
                           "std::common_type"
                           "std::common_reference"
                           "std::basic_common_reference"
                           "std::underlying_type"
                           "std::result_of"
                           "std::invoke_result"
                           "std::void_t"
                           "std::type_identity"
                           "std::conjunction"
                           "std::disjunction"
                           "std::negation"
                           "std::is_pointer_interconvertible_with_class"
                           "std::is_corresponding_member"
                           "std::is_constant_evaluated"
                           ))
   (cons "unordered_map"    (list
                             "std::unordered_map"
                             "std::unordered_multimap"
                             ))
   (cons "unordered_set"    (list
                             "std::unordered_set"
                             "std::unordered_multiset"
                             ))
   (cons "utility"    (list
                       "std::exchange"
                       "std::forward"
                       "std::move_if_noexcept"
                       "std::as_const"
                       "std::declval"
                       "std::cmp_equal"
                       "std::cmp_not_equal"
                       "std::cmp_less"
                       "std::cmp_greater"
                       "std::cmp_less_equal"
                       "std::cmp_greater_equal"
                       "std::in_range"
                       "std::make_pair"
                       "std::pair"
                       "std::integer_sequence"
                       "std::piecewise_construct_t"
                       "std::piecewise_construct"
                       "std::in_place"
                       "std::in_place_type"
                       "std::in_place_index"
                       "std::in_place_t"
                       "std::in_place_type_t"
                       "std::in_place_index_t"
                       ))

   (cons "variant"    (list
                       "std::variant"
                       "std::monostate"
                       "std::bad_variant_access"
                       "std::variant_size"
                       "std::variant_size_v"
                       "std::variant_alternative"
                       "std::variant_alternative_t"
                       "std::variant_npos"
                       "std::visit"
                       "std::holds_alternative"
                       "std::get_if"
                       ))
   )
  "An alist that maps include file names to class names.")



;;
;;  shu-bsl-include-list
;;
(defconst shu-bsl-include-list
  (list
   (cons "bsl_algrithm.h"    (list
                              "bsl::all_of"
                              "bsl::any_of"
                              "bsl::none_of"
                              "bsl::for_each"
                              "bsl::for_each_n"
                              "bsl::count"
                              "bsl::count_if"
                              "bsl::mismatch"
                              "bsl::find"
                              "bsl::find_if"
                              "bsl::find_if_not"
                              "bsl::find_end"
                              "bsl::find_first_of"
                              "bsl::adjacent_find"
                              "bsl::search"
                              "bsl::search_n"
                              "bsl::copy"
                              "bsl::copy_if"
                              "bsl::copy_n"
                              "bsl::copy_backward"
                              "bsl::move"
                              "bsl::move_backward"
                              "bsl::fill"
                              "bsl::fill_n"
                              "bsl::transform"
                              "bsl::generate"
                              "bsl::generate_n"
                              "bsl::remove"
                              "bsl::remove_if"
                              "bsl::remove_copy"
                              "bsl::remove_copy_if"
                              "bsl::replace"
                              "bsl::replace_if"
                              "bsl::replace_copy"
                              "bsl::replace_copy_if"
                              "bsl::swap"
                              "bsl::swap_ranges"
                              "bsl::iter_swap"
                              "bsl::reverse"
                              "bsl::reverse_copy"
                              "bsl::rotate"
                              "bsl::rotate_copy"
                              "bsl::shift_left"
                              "bsl::shift_right"
                              "bsl::random_shuffle"
                              "bsl::shuffle"
                              "bsl::sample"
                              "bsl::unique"
                              "bsl::unique_copy"
                              "bsl::is_partitioned"
                              "bsl::partition"
                              "bsl::partition_copy"
                              "bsl::stable_partition"
                              "bsl::partition_point"
                              "bsl::is_sorted"
                              "bsl::is_sorted_until"
                              "bsl::sort"
                              "bsl::partial_sort"
                              "bsl::partial_sort_copy"
                              "bsl::stable_sort"
                              "bsl::nth_element"
                              "bsl::lower_bound"
                              "bsl::upper_bound"
                              "bsl::binary_search"
                              "bsl::equal_range"
                              "bsl::merge"
                              "bsl::inplace_merge"
                              "bsl::includes"
                              "bsl::set_difference"
                              "bsl::set_intersection"
                              "bsl::set_symmetric_difference"
                              "bsl::set_union"
                              "bsl::is_heap"
                              "bsl::is_heap_until"
                              "bsl::make_heap"
                              "bsl::push_heap"
                              "bsl::pop_heap"
                              "bsl::sort_heap"
                              "bsl::max"
                              "bsl::max_element"
                              "bsl::min"
                              "bsl::min_element"
                              "bsl::minmax"
                              "bsl::minmax_element"
                              "bsl::clamp"
                              "bsl::equal"
                              "bsl::lexicographical_compare"
                              "bsl::lexicographical_compare_three_way"
                              "bsl::is_permutation"
                              "bsl::next_permutation"
                              "bsl::prev_permutation"
                              ))
   (cons "bsl_atomic.h"    (list
                            "bsl::atomic"
                            "bsl::atomic_ref"
                            "bsl::atomic_flag"
                            "bsl::memory_order"
                            "bsl::atomic_bool"
                            "bsl::atomic_char"
                            "bsl::atomic_schar"
                            "bsl::atomic_uchar"
                            "bsl::atomic_short"
                            "bsl::atomic_ushort"
                            "bsl::atomic_int"
                            "bsl::atomic_uint"
                            "bsl::atomic_long"
                            "bsl::atomic_ulong"
                            "bsl::atomic_llong"
                            "bsl::atomic_ullong"
                            "bsl::atomic_char8_t"
                            "bsl::atomic_char16_t"
                            "bsl::atomic_char32_t"
                            "bsl::atomic_wchar_t"
                            "bsl::atomic_int8_t"
                            "bsl::atomic_uint8_t"
                            "bsl::atomic_int16_t"
                            "bsl::atomic_uint16_t"
                            "bsl::atomic_int32_t"
                            "bsl::atomic_uint32_t"
                            "bsl::atomic_int64_t"
                            "bsl::atomic_uint64_t"
                            "bsl::atomic_int_least8_t"
                            "bsl::atomic_uint_least8_t"
                            "bsl::atomic_int_least16_t"
                            "bsl::atomic_uint_least16_t"
                            "bsl::atomic_int_least32_t"
                            "bsl::atomic_uint_least32_t"
                            "bsl::atomic_int_least64_t"
                            "bsl::atomic_uint_least64_t"
                            "bsl::atomic_int_fast8_t"
                            "bsl::atomic_uint_fast8_t"
                            "bsl::atomic_int_fast16_t"
                            "bsl::atomic_uint_fast16_t"
                            "bsl::atomic_int_fast32_t"
                            "bsl::atomic_uint_fast32_t"
                            "bsl::atomic_int_fast64_t"
                            "bsl::atomic_uint_fast64_t"
                            "bsl::atomic_intptr_t"
                            "bsl::atomic_uintptr_t"
                            "bsl::atomic_size_t"
                            "bsl::atomic_ptrdiff_t"
                            "bsl::atomic_intmax_t"
                            "bsl::atomic_uintmax_t"
                            "bsl::atomic_signed_lock_free"
                            "bsl::atomic_unsigned_lock_free"
                            "bsl::atomic_is_lock_free"
                            "bsl::atomic_store"
                            "bsl::atomic_store_explicit"
                            "bsl::atomic_load"
                            "bsl::atomic_load_explicit"
                            "bsl::atomic_exchange"
                            "bsl::atomic_exchange_explicit"
                            "bsl::atomic_compare_exchange_weak"
                            "bsl::atomic_compare_exchange_weak_explicit"
                            "bsl::atomic_compare_exchange_strong"
                            "bsl::atomic_compare_exchange_strong_explicit"
                            "bsl::atomic_fetch_add"
                            "bsl::atomic_fetch_add_explicit"
                            "bsl::atomic_fetch_sub"
                            "bsl::atomic_fetch_sub_explicit"
                            "bsl::atomic_fetch_and"
                            "bsl::atomic_fetch_and_explicit"
                            "bsl::atomic_fetch_or"
                            "bsl::atomic_fetch_or_explicit"
                            "bsl::atomic_fetch_xor"
                            "bsl::atomic_fetch_xor_explicit"
                            "bsl::atomic_wait"
                            "bsl::atomic_wait_explicit"
                            "bsl::atomic_notify_one"
                            "bsl::atomic_notify_all"
                            "bsl::atomic_flag_test"
                            "bsl::atomic_flag_test_explicit"
                            "bsl::atomic_flag_test_and_set"
                            "bsl::atomic_flag_test_and_set_explicit"
                            "bsl::atomic_flag_clear"
                            "bsl::atomic_flag_clear_explicit"
                            "bsl::atomic_flag_wait"
                            "bsl::atomic_flag_wait_explicit"
                            "bsl::atomic_flag_notify_one"
                            "bsl::atomic_flag_notify_all"
                            "bsl::atomic_init"
                            "bsl::kill_dependency"
                            "bsl::atomic_thread_fence"
                            "bsl::atomic_signal_fence"
                            ))
   (cons "bsl_bitset.h"    (list
                            "bsl::bitset"
                            ))
   (cons "bdlf_bind.h"   (list
                          "bdlf::BindUtil"
                          "bdlf::BindWrapper"
                          ))
   (cons "bdlf_placeholder.h"    (list
                                  "bdlf::Placeholders"
                                  ))
   (cons "bsl_chrono.h"    (list
                            "bsl::time_point"
                            "bsl::system_clock"
                            "bsl::steady_clock"
                            "bsl::high_resolution_clock"
                            "bsl::time_point_cast"
                            "bsl::is_am"
                            "bsl::is_pm"
                            "bsl::get_tzdb"
                            "bsl::get_tzdb_list"
                            "bsl::reload_tzdb"
                            "bsl::remote_version"
                            ))
   (cons "bsl_cstddef.h"    (list
                             "bsl::size_t"
                             "bsl::ptrdiff_t"
                             "bsl::max_align_t"
                             "bsl::nullptr_t"
                             ))
   (cons "bsl_functional.h"    (list
                                "bsl::function"
                                "bsl::mem_fn"
                                "bsl::bad_function_call"
                                "bsl::is_bind_expression"
                                "bsl::is_placeholder"
                                "bsl::reference_wrapper"
                                ))
   (cons "bsl_future.h"    (list
                            "bsl::promise"
                            "bsl::packaged_task"
                            "bsl::future"
                            "bsl::shared_future"
                            "bsl::launch"
                            "bsl::future_status"
                            "bsl::future_error"
                            "bsl::future_errc"
                            "bsl::async"
                            "bsl::future_category"
                            ))
   (cons "bsl_iomanip.h"    (list
                             "bsl::resetiosflags"
                             "bsl::setiosflags"
                             "bsl::setbase"
                             "bsl::setfill"
                             "bsl::setprecision"
                             "bsl::setw"
                             "bsl::get_money"
                             "bsl::put_money"
                             "bsl::get_time"
                             "bsl::put_time"
                             "bsl::quoted"
                             ))
   (cons "bsl_iostream.h"    (list
                              "bsl::cin"
                              "bsl::wcin"
                              "bsl::cout"
                              "bsl::wcout"
                              "bsl::cerr"
                              "bsl::wcerr"
                              "bsl::clog"
                              "bsl::wclog"
                              ))
   (cons "bsl_ios.h"    (list
                         "bsl::basic_ios"
                         "bsl::fpos"
                         "bsl::ios"
                         "bsl::ios_base"
                         "bsl::wios"
                         "bsl::io_errc"
                         "bsl::streamoff"
                         "bsl::streampos"
                         "bsl::streamsize"
                         "bsl::wstreampos"
                         "bsl::boolalpha"
                         "bsl::showbase"
                         "bsl::showpoint"
                         "bsl::showpos"
                         "bsl::skipws"
                         "bsl::unitbuf"
                         "bsl::uppercase"
                         "bsl::noboolalpha"
                         "bsl::noshowbase"
                         "bsl::noshowpoint"
                         "bsl::noshowpos"
                         "bsl::noskipws"
                         "bsl::nounitbuf"
                         "bsl::nouppercase"
                         "bsl::dec"
                         "bsl::hex"
                         "bsl::oct"
                         "bsl::fixed"
                         "bsl::scientific"
                         "bsl::internal"
                         "bsl::left"
                         "bsl::right"
                         ))
   (cons "bsl_istream.h"    (list
                             "bsl::basic_istream"
                             "bsl::istream"
                             "bsl::wistream"
                             "bsl::basic_iostream"
                             "bsl::iostream"
                             "bsl::wiostream"
                             "bsl::ws"
                             ))
   (cons "bsl_locale.h"    (list
                            "bsl::narrow"
                            "bsl::tolower"
                            "bsl::toupper"
                            "bsl::widen"
                            ))
   (cons "bsl_limits.h"    (list
                            "bsl::numeric_limits"
                            ))
   (cons "bsl_map.h"    (list
                         "bsl::map"
                         "bsl::multimap"
                         ))
   (cons "bsl_memory.h"    (list
                            "bsl::allocator"
                            "bsl::allocator_arg"
                            "bsl::allocator_arg_t"
                            "bsl::allocator_traits"
                            "bsl::auto_ptr"
                            "bsl::auto_ptr_ref"
                            "bsl::shared_ptr"
                            "bsl::weak_ptr"
                            "bsl::unique_ptr"
                            "bsl::default_delete"
                            "bsl::make_shared"
                            "bsl::allocate_shared"
                            "bsl::static_pointer_cast"
                            "bsl::dynamic_pointer_cast"
                            "bsl::const_pointer_cast"
                            "bsl::get_deleter"
                            "bsl::owner_less"
                            "bsl::enable_shared_from_this"
                            "bsl::raw_storage_iterator"
                            "bsl::get_temporary_buffer"
                            "bsl::return_temporary_buffer"
                            "bsl::uninitialized_copy"
                            "bsl::uninitialized_copy_n"
                            "bsl::uninitialized_fill"
                            "bsl::uninitialized_fill_n"
                            "bsl::uninitialized_move"
                            "bsl::uninitialized_move_n"
                            "bsl::uninitialized_default_construct"
                            "bsl::uninitialized_default_construct_n"
                            "bsl::uninitialized_value_construct"
                            "bsl::uninitialized_value_construct_n"
                            "bsl::destroy"
                            "bsl::destroy_at"
                            "bsl::destroy_n"
                            "bsl::make_unique"
                            "bsl::pointer_traits"
                            "bsl::pointer_safety"
                            "bsl::declare_reachable"
                            "bsl::undeclare_reachable"
                            "bsl::declare_no_pointers"
                            "bsl::undeclare_no_pointers"
                            "bsl::get_pointer_safety"
                            "bsl::align"
                            "bsl::addressof"
                            "bsl::reinterpret_pointer_cast"
                            ))
   (cons "bsl_new.h"   (list
                        "bsl::bad_alloc"
                        "bsl::bad_array_new_length"
                        "bsl::nothrow_t"
                        "bsl::align_val_t"
                        "bsl::new_handler"
                        "bsl::hardware_destructive_interference_size"
                        "bsl::hardware_constructive_interference_size"
                        "bsl::get_new_handler"
                        "bsl::set_new_handler"
                        "bsl::launder"
                        ))
   (cons "bsl_optional.h"   (list
                             "bsl::optional"
                             "bsl::bad_optional_access"
                             "bsl::nullopt_t"
                             "bsl::nullopt"
                             "bsl::make_optional"
                             ))
   (cons "bsl_ostream.h"   (list
                            "bsl::basic_ostream"
                            "bsl::ostream"
                            "bsl::wostream"
                            "bsl::endl"
                            "bsl::ends"
                            "bsl::flush"
                            "bsl::emit_on_flush"
                            "bsl::noemit_on_flush"
                            "bsl::flush_emit"
                            ))
   (cons "bsl_queue.h"   (list
                          "bsl::queue"
                          "bsl::priority_queue"
                          ))
   (cons "bsl_regex.h"   (list
                          "bsl::basic_regex"
                          "bsl::sub_match"
                          "bsl::match_results"
                          "bsl::regex_iterator"
                          "bsl::regex_token_iterator"
                          "bsl::regex_error"
                          "bsl::regex_traits"
                          "bsl::regex_match"
                          "bsl::regex_search"
                          "bsl::regex_replace"
                          ))
   (cons "bsl_set.h"   (list
                        "bsl::set"
                        "bsl::multiset"
                        ))
   (cons "bsl_sstream.h"   (list
                            "bsl::istringstream"
                            "bsl::ostringstream"
                            "bsl::stringbuf"
                            "bsl::stringstream"
                            "bsl::wistringstream"
                            "bsl::wostringstream"
                            "bsl::wstringbuf"
                            "bsl::wstringstream"
                            ))
   (cons "bsl_stdexcept.h"    (list
                               "bsl::domain_error"
                               "bsl::invalid_argument"
                               "bsl::length_error"
                               "bsl::logic_error"
                               "bsl::out_of_range"
                               "bsl::overflow_error"
                               "bsl::range_error"
                               "bsl::runtime_error"
                               "bsl::underflow_error"
                               ))
   (cons "bsl_string.h"   (list
                           "bsl::basic_string"
                           "bsl::char_traits"
                           "bsl::string"
                           "bsl::u16string"
                           "bsl::wstring"
                           "bsl::stoi"
                           "bsl::stol"
                           "bsl::stoul"
                           "bsl::stoll"
                           "bsl::stoull"
                           "bsl::stof"
                           "bsl::stod"
                           "bsl::stold"
                           "bsl::to_string"
                           "bsl::to_wstring"
                           ))
   (cons "bsl_string_view.h"   (list
                                "bsl::string_view"
                                "bsl::u8string_view"
                                "bsl::u16string_view"
                                "bsl::u32string_view"
                                "bsl::wstring_view"
                                ))
   (cons "bsl_tuple.h"    (list
                           "bsl::tuple"
                           "bsl::tuple_size"
                           "bsl::tuple_element"
                           "bsl::make_tuple"
                           "bsl::forward_as_tuple"
                           "bsl::tie"
                           "bsl::tuple_cat"
                           "bsl::get"
                           "bsl::ignore"
                           ))
   (cons "bsl_type_traits.h"    (list
                                 "bsl::integral_constant"
                                 "bsl::bool_constant"
                                 "bsl::true_type"
                                 "bsl::false_type"
                                 "bsl::is_void"
                                 "bsl::is_null_pointer"
                                 "bsl::is_integral"
                                 "bsl::is_floating_point"
                                 "bsl::is_array"
                                 "bsl::is_enum"
                                 "bsl::is_union"
                                 "bsl::is_class"
                                 "bsl::is_function"
                                 "bsl::is_pointer"
                                 "bsl::is_lvalue_reference"
                                 "bsl::is_rvalue_reference"
                                 "bsl::is_member_object_pointer"
                                 "bsl::is_member_function_pointer"
                                 "bsl::is_fundamental"
                                 "bsl::is_arithmetic"
                                 "bsl::is_scalar"
                                 "bsl::is_object"
                                 "bsl::is_compound"
                                 "bsl::is_reference"
                                 "bsl::is_member_pointer"
                                 "bsl::is_const"
                                 "bsl::is_volatile"
                                 "bsl::is_trivial"
                                 "bsl::is_trivially_copyable"
                                 "bsl::is_standard_layout"
                                 "bsl::is_pod"
                                 "bsl::has_unique_object_representations"
                                 "bsl::is_empty"
                                 "bsl::is_polymorphic"
                                 "bsl::is_abstract"
                                 "bsl::is_final"
                                 "bsl::is_aggregate"
                                 "bsl::is_signed"
                                 "bsl::is_unsigned"
                                 "bsl::is_bounded_array"
                                 "bsl::is_unbounded_array"
                                 "bsl::is_scoped_enum"
                                 "bsl::is_constructible"
                                 "bsl::is_trivially_constructible"
                                 "bsl::is_nothrow_constructible"
                                 "bsl::is_default_constructible"
                                 "bsl::is_trivially_default_constructible"
                                 "bsl::is_nothrow_default_constructible"
                                 "bsl::is_copy_constructible"
                                 "bsl::is_trivially_copy_constructible"
                                 "bsl::is_nothrow_copy_constructible"
                                 "bsl::is_move_constructible"
                                 "bsl::is_trivially_move_constructible"
                                 "bsl::is_nothrow_move_constructible"
                                 "bsl::is_assignable"
                                 "bsl::is_trivially_assignable"
                                 "bsl::is_nothrow_assignable"
                                 "bsl::is_copy_assignable"
                                 "bsl::is_trivially_copy_assignable"
                                 "bsl::is_nothrow_copy_assignable"
                                 "bsl::is_move_assignable"
                                 "bsl::is_trivially_move_assignable"
                                 "bsl::is_nothrow_move_assignable"
                                 "bsl::is_destructible"
                                 "bsl::is_trivially_destructible"
                                 "bsl::is_nothrow_destructible"
                                 "bsl::has_virtual_destructor"
                                 "bsl::is_swappable_with"
                                 "bsl::is_swappable"
                                 "bsl::is_nothrow_swappable_with"
                                 "bsl::is_nothrow_swappable"
                                 "bsl::alignment_of"
                                 "bsl::rank"
                                 "bsl::extent"
                                 "bsl::is_same"
                                 "bsl::is_base_of"
                                 "bsl::is_convertible"
                                 "bsl::is_nothrow_convertible"
                                 "bsl::is_layout_compatible"
                                 "bsl::is_pointer_interconvertible_base_of"
                                 "bsl::is_invocable"
                                 "bsl::is_invocable_r"
                                 "bsl::is_nothrow_invocable"
                                 "bsl::is_nothrow_invocable_r"
                                 "bsl::remove_cv"
                                 "bsl::remove_const"
                                 "bsl::remove_volatile"
                                 "bsl::add_cv"
                                 "bsl::add_const"
                                 "bsl::add_volatile"
                                 "bsl::remove_reference"
                                 "bsl::add_lvalue_reference"
                                 "bsl::add_rvalue_reference"
                                 "bsl::remove_pointer"
                                 "bsl::add_pointer"
                                 "bsl::make_signed"
                                 "bsl::make_unsigned"
                                 "bsl::remove_extent"
                                 "bsl::remove_all_extents"
                                 "bsl::aligned_storage"
                                 "bsl::aligned_union"
                                 "bsl::decay"
                                 "bsl::remove_cvref"
                                 "bsl::enable_if"
                                 "bsl::conditional"
                                 "bsl::common_type"
                                 "bsl::common_reference"
                                 "bsl::basic_common_reference"
                                 "bsl::underlying_type"
                                 "bsl::result_of"
                                 "bsl::invoke_result"
                                 "bsl::void_t"
                                 "bsl::type_identity"
                                 "bsl::conjunction"
                                 "bsl::disjunction"
                                 "bsl::negation"
                                 "bsl::is_pointer_interconvertible_with_class"
                                 "bsl::is_corresponding_member"
                                 "bsl::is_constant_evaluated"
                                 ))
   (cons "bsl_unordered_map.h"    (list
                                   "bsl::unordered_map"
                                   "bsl::unordered_multimap"
                                   ))
   (cons "bsl_unordered_set.h"    (list
                                   "bsl::unordered_set"
                                   "bsl::unordered_multiset"
                                   ))
   (cons "bsl_utility.h"    (list
                             "bsl::exchange"
                             "bsl::forward"
                             "bsl::move_if_noexcept"
                             "bsl::as_const"
                             "bsl::declval"
                             "bsl::cmp_equal"
                             "bsl::cmp_not_equal"
                             "bsl::cmp_less"
                             "bsl::cmp_greater"
                             "bsl::cmp_less_equal"
                             "bsl::cmp_greater_equal"
                             "bsl::in_range"
                             "bsl::make_pair"
                             "bsl::pair"
                             "bsl::integer_sequence"
                             "bsl::piecewise_construct_t"
                             "bsl::piecewise_construct"
                             "bsl::in_place"
                             "bsl::in_place_type"
                             "bsl::in_place_index"
                             "bsl::in_place_t"
                             "bsl::in_place_type_t"
                             "bsl::in_place_index_t"
                             ))

   (cons "bsl_variant.h"    (list
                             "bsl::variant"
                             "bsl::monostate"
                             "bsl::bad_variant_access"
                             "bsl::variant_size"
                             "bsl::variant_size_v"
                             "bsl::variant_alternative"
                             "bsl::variant_alternative_t"
                             "bsl::variant_npos"
                             "bsl::visit"
                             "bsl::holds_alternative"
                             "bsl::get_if"
                             ))

   (cons "bsls_atomic.h"   (list
                            "bsls::AtomicBool"
                            "bsls::AtomicInt"
                            "bsls::AtomicInt64"
                            "bsls::AtomicUint"
                            "bsls::AtomicUint64"
                            "bsls::AtomicPointer"
                            ))
   (cons "bdldfp_decimal.h"   (list
                               "bdldfp::Decimal32"
                               "bdldfp::Decimal64"
                               "bdldfp::Decimal128"
                               "bdldfp::DecimalNumGet"
                               ))
   (cons "mbsa_session.h"    (list
                              "mbsa::Session"
                              "mbsa::SessionEventHandler"
                              ))
   )
  "An alist that maps include file names to class names when using BDE.")




;;
;;  Functions for customzing
;;


;;
;;  shu-add-cpp-base-types
;;
(defun shu-add-cpp-base-types (ntypes)
  "Add one or more data types to the list of C++ native data types defined in shu-cpp-base-types
in shu-cpp-general.el.  Argument may be a single type in a string or a list of strings.
This modifies shu-cpp-base-types."
  (let ((nt ntypes))
    (when (not (listp nt))
      (setq nt (list nt)))
    (setq shu-cpp-base-types (append shu-cpp-base-types nt))
    ))



;;
;;  shu-cpp-map-class-to-include
;;
(defun shu-cpp-map-class-to-include (class-name)
  "CLASS-NAME is a fully qualified class name (std::string as an example).  This
function returns the name of the include file that defines the class, if known."
  (let ((ret-val)
        (file-name))
    (when (not shu-cpp-include-names)
      (if shu-cpp-use-bde-library
          (setq ret-val (shu-invert-alist-to-hash shu-bsl-include-list))
        (setq ret-val (shu-invert-alist-to-hash shu-std-include-list)))
      (setq shu-cpp-include-names (car ret-val)))
    (setq file-name (gethash class-name shu-cpp-include-names))
    file-name
    ))



;;
;;  shu-cpp1-class
;;
(defun shu-cpp1-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (interactive "*sClass name?: ")
  (let ((debug-on-error t))
    (insert (concat
             "class " class-name "\n"
             "{\n"
             "public:\n\n"
             "  " class-name "()\n"
             "   { }\n\n"
             "private:\n\n"
             "  " class-name "(\n"
             "    const " class-name "   &rhs);\n\n"
             "  " class-name " &operator=(\n"
             "    const " class-name "   &rhs);\n\n"
             "};"))
    (search-backward "{" nil t)
    (forward-char 2)
    ))


;;
;;  shu-cpp2-class
;;
(defun shu-cpp2-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (interactive "*sClass name?: ")
  (shu-internal-cpp2-class class-name)
  )


;;
;;  shu-new-c-class
;;
(defun shu-new-c-class ()
  "Place a skeleton class definition in the current buffer at point."
  (interactive)
  (let ((debug-on-error t)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (shu-internal-cpp2-class hfile-base)
    ))

;;
;;  shu-internal-cpp2-class
;;
(defun shu-internal-cpp2-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length  0)
        (ostream        "std::ostream")
        (otry           nil)
        (opt-length     22)
        (class-arg      nil)
        (arg-length      0)
        (pad            "")
        (pad-after      "  ")
        (pad-count      0)
        (comment1       "!< The stream into which we stream")
        (comment2       "!< The object to be streamed"))

    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 3))

                                        ; std::ostream                &os
                                        ; const required_item_thing   &cn
                                        ; 123456789012345678901234567890

    (setq class-arg (concat "const " class-name))
    (setq arg-length (max (length class-arg) (length ostream)))
    (when (< arg-length opt-length)
      (setq pad-after (concat pad-after " ")))
    (setq class-arg (concat class-arg pad-after))
    (setq ostream (concat ostream pad-after))
    (setq arg-length (max (length class-arg) (length ostream)))
    (setq class-arg (shu-make-padded-line class-arg arg-length))
    (setq ostream (shu-make-padded-line ostream arg-length))

    (setq pad-count (- comment-start (length ostream) 9))
    (if (> pad-count 0)
        (setq pad (make-string pad-count ? ))
                                        ;
      (setq comment-length (+ comment-length pad-count)))
    (setq comment1 (shu-make-padded-line comment1 comment-length))
    (setq comment2 (shu-make-padded-line comment2 comment-length))

                                        ;  (when (< (length ostream) (length class-arg))
                                        ;    (setq class-arg (shu-make-padded-line class-arg (length ostream))))

    (insert (concat
             "/*!\n"
             " * \\brief Description of " class-name "\n"
             " */\n"
             "class " class-name "\n"
             "{\n"
             "public:\n\n"
             "  " "/*!\n"
             "  " " * \\brief Standard constructor\n"
             "  " " */\n"
             "  explicit " class-name "()\n"
             "  { }\n\n\n"
             "  " "/*!\n"
             "  " " *  \\brief Stream object out to a stream\n"
             "  " " *\n"
             "  " " * \\return The same stream as the input to allow for chained operators.\n"
             "  " " */\n"
             "  friend std::ostream &operator<<(\n"
             "    " ostream "&os," pad "/*" comment1 "*/\n"
             "    " class-arg "&cn)" pad "/*" comment2 "*/\n"
             "  {\n"
             "    return cn.print_self(os);\n"
             "  }\n\n\n"
             "private:\n\n"
             "  " "/*!\n"
             "  " " * \\brief The copy constructor is deliberately private and unimplemented.\n"
             "  " " *\n"
             "  " " * \\param rhs the object from which we are to be constructed\n"
             "  " " */\n"
             "  " class-name "(\n"
             "    const " class-name "   &rhs);\n\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator=() is deliberately private and unimplemented.\n"
             "  " " *\n"
             "  " " * \\param rhs the object from which we are to be assigned\n"
             "  " " *\n"
             "  " " * \\return reference to self to allow for chained operators\n"
             "  " " */\n"
             "  " class-name " &operator=(\n"
             "    const " class-name "   &rhs);\n\n\n"
             "  " "/*!\n"
             "  " " * \\brief This is the implementation function for operator<<()\n"
             "  " " *\n"
             "  " " * \\return The same stream as the input to allow for chained operators.\n"
             "  " " */\n"
             "  std::ostream &print_self(\n"
             "    std::ostream    &os)           /*!< The stream into which we stream     */\n"
             "  const\n"
             "  {\n"
             "    os << \"" class-name "\";\n\n"
             "    return os;\n"
             "  }\n\n\n"
             "};"))
    (search-backward "class " nil t)
    (search-backward "brief" nil t)
    (end-of-line)
    ))

;;
;;  shu-operators
;;
;;  Put skeletons for all of the operator functions into a header file
;;
(defun shu-operators (class-name)
  "Place skeletons of all of the standard c++ operator functions at point."
  (interactive "*sClass name?: ")
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length  0)
        (ostream        "std::ostream")
        (otry           nil)
        (opt-length     22)
        (class-arg      nil)
        (arg-length      0)
        (pad            "")
        (pad-after      "  ")
        (pad-count      0)
        (str   )
        (comment1       "!< The stream into which we stream")
        (comment2       "!< The object to be streamed"))

    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 3))

                                        ; std::ostream                &os
                                        ; const required_item_thing   &cn
                                        ; 123456789012345678901234567890

    (setq class-arg (concat "const " class-name))
    (setq arg-length (max (length class-arg) (length ostream)))
    (when (< arg-length opt-length)
      (setq pad-after (concat pad-after " ")))
    (setq class-arg (concat class-arg pad-after))
    (setq ostream (concat ostream pad-after))
    (setq arg-length (max (length class-arg) (length ostream)))
    (setq class-arg (shu-make-padded-line class-arg arg-length))
    (setq ostream (shu-make-padded-line ostream arg-length))

    (setq pad-count (- comment-start (length ostream) 9))
    (if (> pad-count 0)
        (setq pad (make-string pad-count ? ))
                                        ;
      (setq comment-length (+ comment-length pad-count)))
    (setq comment1 (shu-make-padded-line comment1 comment-length))
    (setq comment2 (shu-make-padded-line comment2 comment-length))

                                        ;  (when (< (length ostream) (length class-arg))
                                        ;    (setq class-arg (shu-make-padded-line class-arg (length ostream))))
    (setq str (concat "  " "  const " class-name "   &rhs"))
    (setq pad-count (- comment-start (length str) 2 ))
    (when (> pad-count 0)
      (setq pad (make-string pad-count ? )))
    (insert (concat
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator+()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator+(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to add           */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator+=()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator+=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to add           */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator-()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator-(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to subtract      */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator-=()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator-=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to subtract      */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator<()\n"
             "  " " *\n"
             "  " " * \\return true if this object is less than the right hand one\n"
             "  " " */\n"
             "  " " bool  operator<(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator<=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is less than or equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator<=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator>()\n"
             "  " " *\n"
             "  " " * \\return true if this object is greater than the right hand one\n"
             "  " " */\n"
             "  " " bool  operator>(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator>=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is greater than or equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator>=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator==()\n"
             "  " " *\n"
             "  " " * \\return true if this object is equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator==(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator!=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is not equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator!=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = !operator==(rhs);\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             ))

    (search-backward "class " nil t)
    (search-backward "brief" nil t)
    (end-of-line)
    ))


;;
;;  shu-set-author
;;
(defun shu-set-author (name)
  "Set the author name to be placed in generated C++ classes."
  (interactive)
  (setq shu-cpp-author name))



;;
;;  shu-set-modern
;;
(defun shu-set-modern ()
  "Unconditionally set shu-cpp-modern to true."
  (interactive)
  (let ((msg))
    (if shu-cpp-modern
        (setq msg "shu-cpp-modern already true")
      (setq shu-cpp-modern t)
      (setq msg "shu-cpp-modern set to true"))
    (message "%s" msg)
    ))



;;
;;  shu-set-no-modern
;;
(defun shu-set-no-modern ()
  "Unconditionally set shu-cpp-modern to false."
  (interactive)
  (let ((msg))
    (if (not shu-cpp-modern)
        (setq msg "shu-cpp-modern already false")
      (setq shu-cpp-modern nil)
      (setq msg "shu-cpp-modern set to false"))
    (message "%s" msg)
    ))



;;
;;  shu-set-default-global-namespace
;;
(defun shu-set-default-global-namespace (name)
  "Set the global namespace for C++ classes."
  (interactive "sName?: ")
  (setq shu-cpp-default-global-namespace name))


;;
;;  shu-set-default-namespace
;;
(defun shu-set-default-namespace (name)
  "Set the local namespace for C++ classes."
  (interactive "sName?: ")
  (setq shu-cpp-default-namespace name))


;;
;;  shu-dox-hdr
;;
(defun shu-dox-hdr ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \n"
             pad " */"
             ))
    (search-backward "*/")
    (search-backward "*")
    (forward-char 2)
    ))

;;
;;  shu-dox-brief
;;
(defun shu-dox-brief ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief \n"
             pad " */"
             ))
    (search-backward "*/")
    (search-backward "*")
    (search-forward "brief")
    (forward-char 1)
    ))

;;
;;  shu-dox2-hdr
;;
(defun shu-dox2-hdr ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief \n"
             pad " * \n"
             pad " */"
             ))
    (search-backward "brief")
    (search-forward " ")
    ))

;;
;;  shu-dcc
;;
(defun shu-dcc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief The copy constructor is deliberately private and unimplemented.\n"
             pad " */"
             ))
    ))

;;
;;  shu-dce
;;
(defun shu-dce ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief operator=() is deliberately private and unimplemented.\n"
             pad " */"
             ))
    ))

;;
;;  shu-clc
;;
(defun shu-clc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * The constructor acquires the latch.\n"
             pad " */"
             ))
    ))

;;
;;  shu-drc
;;
(defun shu-drc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * The destructor releases the latch.\n"
             pad " */"
             ))
    ))



;;
;;  shu-binclude
;;
(defun shu-binclude ()
  "If point is sitting on something that resembles a fully qualified class name,
first check to see if it is in list of standard class names defined in
SHU-CPP-INCLUDE-NAMES.  If it is found there, that defines the name of the
defining include file.  If it is not found there, then use the standard BDE
algorithm to turn the class name into the name of an include file.  The standard
BDE algorithm replaces the :: between namespace and class name with an
underscore, makes all letters lower case, and appends \".h\" to the end of the
name.

Thus \"abcdef::MumbleFrotz\" becomes \"abcdef_mumblefrotz.h\".

An include directive for the file is then created and put into the kill ring for
a subsequent yank.

The file name is delimited by double quotes unless SHU-CPP-INCLUDE-USER-BRACKETS
variable is true, in which case the file name is delimited by left and right
angle brackets.

Return true if a class name was found an an include generated.  This is for the
benefit of unit tests."
  (interactive)
  (let* ((gb (get-buffer-create "**foo**"))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (target-list (append shu-cpp-name-list (list ":")))
         (target-char (regexp-opt target-list nil))
         (target-name
          (concat
           "\\(" shu-cpp-name "+\\)"
           "::"
           "\\(" shu-cpp-name "+\\)"))
         (namespace)
         (class-name)
         (qualified-name)
         (file-name)
         (include-name)
         (left-delim (if shu-cpp-include-user-brackets "<" "\""))
         (right-delim (if shu-cpp-include-user-brackets ">" "\""))
         (got-it))
    (save-excursion
      (if (not (looking-at target-char)) ;; Looking at a legal class name character
          (message "%s" "Not a properly formed class name")
        (while (and (looking-at target-char) ;; Still on a file name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a file name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (if (not (re-search-forward target-name eol t))
            (message "%s" "Not a properly formed class name")
          (setq namespace (match-string 1)) ;; Have something that matches file name syntax
          (setq class-name (match-string 2))
          (setq qualified-name (concat namespace "::" class-name))
          (princ (concat "qualified-name: " qualified-name "\n") gb)
          (setq file-name (shu-cpp-map-class-to-include qualified-name))
          (when (not file-name)
            (setq file-name (concat
                             (downcase namespace) "_"
                             (downcase class-name) ".h")))
          (setq include-name (concat
                              "#include "
                              left-delim file-name right-delim))
          (message "%s" include-name)
          (shu-kill-new include-name)
          (setq got-it t))))
    got-it
    ))



;;
;;  shu-ginclude
;;
(defun shu-ginclude()
  "While in a file buffer, wrap the file name in a C++ include directive and
put it in the kill ring.  The file name is delimited by double quotes unless
SHU-CPP-INCLUDE-USER-BRACKETS variable is true, in which case the file name
is delimited by left and right angle brackets."
  (interactive)
  (let ((name  (file-name-nondirectory (buffer-file-name)))
        (ext)
        (left-delim (if shu-cpp-include-user-brackets
                        "<"
                      "\""))
        (right-delim (if shu-cpp-include-user-brackets
                         ">"
                       "\""))
        (incl))
    (if (not name)
        (ding)
      (setq ext (file-name-extension name))
      (if (not (string= "H" (upcase ext)))
          (progn
            (ding)
            (message "%s" "THIS IS NOT A HEADER FILE"))
        (setq incl (concat "#include "
                           left-delim name right-delim "\n"))
        (shu-kill-new incl)
        (message "%s" incl)))
    ))


;;
;; shu-dox-cvt
;;
(defun shu-dox-cvt ()
  "Convert a section of comments delimited by // into Doxygen format."
  (interactive)
  (let ((debug-on-error t)
        (pad            nil)
        (eol            nil)
        (spos           nil)
        (slash-pos      nil)
        (not-done       t))
    (beginning-of-line)
    (setq eol (save-excursion (forward-line 1) (end-of-line) (point)))
    (setq spos (search-forward "//" eol t))
    (if (not spos)
        (error "Unable to find any // near here")
      (progn
        (setq slash-pos (- (current-column) 2))
        (when (< slash-pos 0)
          (setq slash-pos 0))
        (setq pad (make-string slash-pos ? ))
        (forward-line -1)
        (insert (concat "\n" pad "/*!"))
        (while not-done
          (forward-line 1)
          (setq eol (save-excursion (end-of-line) (point)))
          (if (search-forward "//" eol t)
              (replace-match " *")
            (progn
              (setq not-done nil)
              (insert (concat pad " */\n")))))))
    ))


;;
;; shu-dox-cbt
;;
(defun shu-dox-cbt ()
  "Convert a section of comments delimited by //! into Doxygen brief format."
  (interactive)
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (pad  )
        (eol  )
        (spos )
        (slash-pos )
        (not-done       t)
        (j              0))
    (beginning-of-line)
    (setq eol (save-excursion (forward-line 1) (end-of-line) (point)))
    (setq spos (search-forward "//!" eol t))
    (if (not spos)
        (error "Unable to find any //! near here")
      (progn
        (setq slash-pos (- (current-column) 2))
        (when (< slash-pos 1)
          (setq slash-pos 1))
        (setq pad (make-string (1- slash-pos) ? ))
        (replace-match (concat "/*!\n" pad " * \\brief") t t)
        (beginning-of-line)
        (end-of-line)
        (insert (concat "\n" pad " */"))
        (forward-line -1)
        (end-of-line)))
    ))


;;
;;  shu-new-x-file
;;
(defun shu-new-x-file ()
  "Generate a skeleton Doxygen \\file directive."
  (interactive)
  (let ((xfile (file-name-nondirectory (buffer-file-name))))
    (insert (concat
             "\n"
             "/*!\n"
             "  \\file " xfile "\n"
             "\n"
             "    \\brief Contains the implementation fuctions of BloombergLP::m_moalrt::\n"
             "*/\n"))
    ))


;;
;;  shu-new-h-file
;;
(defun shu-new-h-file ()
  "Generate a skeleton header file for C or C++ file."
  (interactive)
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length nil)
        (comment-length nil)
        (pad-count      nil)
        (pad            nil)
        (ecomment       nil)
        (tcomment       nil)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (file-line  "/*  FILE_NAME: ")
        (slength        nil)
        (incl-name      nil))
    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 4))


    (setq pad-count (- comment-end 4))
    (setq slength (- comment-end 2))
    (setq pad (make-string pad-count ? ))
    (setq ecomment (concat "/*" pad "*/\n"))
    (setq pad (make-string pad-count ?*))
    (setq tcomment (concat "/*" pad "*/\n"))
    (setq file-line (concat file-line hfile))
    (setq pad-count (- comment-end 2 (length file-line)))
    (setq pad (make-string pad-count ? ))
    (setq file-line (concat file-line pad "*/\n"))
    (setq incl-name (concat hfile-base "_h_included"))
    (insert (concat "#ifndef " incl-name "\n"))
    (insert (concat "#define " incl-name " 1\n"))
    (insert "\n")
    (insert "/*!\n")
    (insert (concat " * \\file " hfile "\n"))
    (insert " *\n")
    (insert (concat " * \\brief Contains the definition of " hfile-base "\n"))
    (insert " *\n")
    (insert " * \\author " shu-cpp-author " \n")
    (insert " */\n")
    (insert "\n")
    (insert "#include <iostream>\n")
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert (concat (shu-make-padded-line "#endif" (1- comment-start)) "/* "))
    (insert (concat (shu-make-padded-line incl-name comment-length) "*/\n"))
    (forward-line -3)
    ))

;;
;;  shu-new-c-file
;;
(defun shu-new-c-file ()
  "Generate a skeleton code file for a C or C++ file."
  (interactive)
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length nil)
        (comment-length nil)
        (pad-count      nil)
        (pad            nil)
        (ecomment       nil)
        (tcomment       nil)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (file-line  "/*  FILE_NAME: ")
        (slength        nil)
        (incl-name      nil)
        (base-name (file-name-sans-extension (buffer-file-name))))
    (setq incl-name (concat hfile-base ".h"))
    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 4))


    (setq pad-count (- comment-end 4))
    (setq slength (- comment-end 2))
    (setq pad (make-string pad-count ? ))
    (setq ecomment (concat "/*" pad "*/\n"))
    (setq pad (make-string pad-count ?*))
    (setq tcomment (concat "/*" pad "*/\n"))
    (setq file-line (concat file-line hfile))
    (setq pad-count (- comment-end 2 (length file-line)))
    (setq pad (make-string pad-count ? ))
    (setq file-line (concat file-line pad "*/\n"))
    (insert "\n")
    (insert "/*!\n")
    (insert (concat " * \\file " hfile "\n"))
    (insert " *\n")
    (insert (concat " * \\brief Contains the implementation of " hfile-base "\n"))
    (insert " *\n")
    (insert " * \\author " shu-cpp-author "\n")
    (insert " */\n")
    (insert "\n")
    (insert (concat "#include \"" incl-name "\""))
    (insert "\n")
    (insert "\nusing namespace ::std;\n")
    ;;  (forward-line -3)
    ))

;;
;;  shu-author
;;
(defun shu-author ()
  "Insert the doxygen author tag in an existing file."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (insert " *\n")
  (insert " * \\author " shu-cpp-author "\n")
  )

;;
;;  shu-make-padded-line
;;
(defun shu-make-padded-line (line tlen)
  "Add sufficient spaces to make LINE the length TLEN."
  (let ((clen       (length line))
        (pad        "")
        (pad-count  nil))
    (setq pad-count (- tlen clen))
    (when (> pad-count 0)
      (setq pad (make-string pad-count ? ))  )
    (concat line pad)
    ))

;;
;;  shu-getters
;;
(defun shu-getters (start end)
  "Mark a region in a file that contains C++ instance variable declarations.
This function will create get and set functions for all of the instance
variables."
  (interactive "r")                 ; bounds of marked region (start end)
  (save-excursion
    (let ((sline (shu-the-line-at start)) ; Remember start line
          (eline (shu-the-line-at end))   ; Remember end line
          (line-diff 0)               ; The difference between the number of lines we
                                        ;  tried to move and the number we actually moved
          (eol       nil)
          (dir-name  nil)
          ;;        (shu-cpp-buffer (get-buffer-create shu-cpp-buffer-name))
          (m (copy-marker 300 ) ) )
      (setq shu-cpp-project-list nil)
      (goto-char start)               ; Move to region start
                                        ; While we have not reached last line and
      (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
        (setq eol (save-excursion (end-of-line) (point)))
        (when (> eol (point))
          (get-set)
          )
        (setq line-diff (forward-line 1))
        )
      )
    )
  )

;;
;;  shu-get-set - Emit getters and setters for an instance variable
;;
(defun shu-get-set ()
  "Generate get and set functions for an instance variable in a C++ class.
Position the cursor ahead of the Doxygen comment above the variable.  The get
and set functions will be placed in the buffer *get-set*."
  (interactive)
  (let ((non-white1 "[^[:space:]\n\t\v\f]")
        (non-white2 "[^[:space:]]")
        (cstart )        ;; Comment start position
        (cend )          ;; Comment end position
        (comment "")     ;; Original comment
        (shu-lc-comment "")  ;; Comment with first letter downcased
        )
    (when (re-search-forward non-white1 nil t)
      (backward-char 1)
      (when (looking-at "//!")
        (forward-char 3)
        (re-search-forward non-white2 nil t)
        (setq cstart (1- (point)))
        (setq cend (save-excursion (end-of-line) (point)))
        (setq comment (buffer-substring cstart cend))
        (setq cstart (substring comment 0 1))
        (setq cstart (downcase cstart))
        (setq shu-lc-comment (concat cstart (substring comment 1)))
        (forward-line 1)
        )
      (shu-internal-get-set comment shu-lc-comment)
      )
    ))


;;
;;  shu-internal-get-set - Emit getters and setters for a instance variable
;;
;; This code assumes that the declaration fits entirely on the current line.  It
;; first scans the line to find the terminating semi-colon.  Then is scans backwards
;; to find the variable name and anything of interest in front of it.  The things
;; that might be in front of it are:
;;
;; *      - This is a pointer
;; &      - This is a reference
;; *const - This is a const pointer
;;
;; Spaces are allowed where one might expect.  The following declarations are fine:
;;
;;  foo * const _bar;
;;  foo  _bar;
;;  foo   *   _bar;
;;  foo    &_bar;
;;
(defun shu-internal-get-set (comment shu-lc-comment)
  "Generate get and set functions for an instance variable in a C++ class."
  (interactive)
  (let (
        ;;    (gbuf      (get-buffer-create shu-unit-test-buffer))
        (gs-buf    (get-buffer-create "*get-set*"))
        (cbuf (current-buffer))
        (bol (save-excursion (beginning-of-line) (point)))
        (eol (save-excursion (end-of-line) (point)))
        (got-semi )   ; True if we found semi-colon at end of declaration
        (name-exp (concat "\\s-*" shu-cpp-name))
        (op (regexp-opt (list "*" "&") nil))
        (op-or-space (regexp-opt (list " " "*" "&") nil))
        (cnx "\\*+\\s-*const\\s-*")
        (name-end )   ; Point at end of attribute name
        (name-start ) ; Point at start of attribute name
        (shu-attr-name )  ; Name of attribute
        (shu-var-name )   ; Corresponding variable name
        (found-op )   ; Set to "*" or "&" if this is a pointer or reference
        (vtype )      ; Data type of attribute
        (shu-nc-vtype )   ; Non-const data type
        (non-blank "[^ \t]")
        (vlist )
        (tlist )
        (tblank )
        (nc-tblank )
        (tword )
        (is-base-type ) ; True if base type (int, long, double, etc)
        (shu-is-const )     ; True if declared to be const
        (shu-is-const-ptr ) ; True if a const pointer (*const x;)
        (mpfx   shu-cpp-member-prefix)
        (mpfx-len (length shu-cpp-member-prefix))
        (debug-on-error t))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward non-blank bol t)
        (setq bol (point)))
      (setq got-semi (search-forward ";" eol t))
      (if (not got-semi)
          (error "%s" "No terminating semi-colon found.")
        (backward-char 1)
        (re-search-backward name-exp bol t)
        (setq name-end (1+ (point)))
        (re-search-backward op-or-space bol t) ;; Look for beginning of name
        (setq name-start (1+ (point)))
        (setq shu-attr-name (buffer-substring name-start name-end))
        (setq shu-var-name shu-attr-name)
        (when (string= (substring shu-attr-name 0 mpfx-len) mpfx)
          (setq shu-var-name (substring shu-attr-name mpfx-len)))
        (if (looking-at op) ;; Name immediately preceded by "*" or "&"
            (progn
              (setq found-op (char-after (point)))
              (re-search-backward non-blank bol t)
              (setq eol (1+ (point)))
              )
          ;; Name immediately preceded by space
          (if (re-search-backward cnx bol t)
              (progn  ;; This is a const pointer (*const x;)
                (setq found-op ?*)
                (setq shu-is-const-ptr t)
                (when (re-search-backward non-blank bol t)
                  (setq eol (1+ (point))))
                )
            (when (re-search-backward non-blank bol t)
              (if (not (looking-at op))
                  (setq eol (1+ (point)))
                (setq found-op (char-after (point)))
                (when (re-search-backward non-blank bol t)
                  (setq eol (1+ (point))))
                )
              )
            )
          )
        (when found-op
          (setq found-op (char-to-string found-op)))
        ;; Fetch the data type and split it into a list of words (eg const unsigned int)
        (setq vtype (buffer-substring bol eol))
        (setq vlist (split-string vtype "[ \t]+" t))
        ;;      (princ vlist (get-buffer gbuf))
        (setq vtype nil)
        (setq tblank "")
        (setq nc-tblank "")
        (setq tlist vlist)
        ;; Check to see if it is const and whether it is a base type (int, long, etc)
        (while tlist
          (setq tword (car tlist))
          (when (string= tword "const")
            (setq shu-is-const t))
          (when (member tword shu-cpp-base-types)
            (setq is-base-type t))
          (setq tlist (cdr tlist))
          )
        ;; Turn the list of words back into a type declaration
        (setq tlist vlist)
        (while tlist
          (setq tword (car tlist))
          (when  (not (string= tword "const"))
            (setq shu-nc-vtype (concat shu-nc-vtype nc-tblank tword))
            (setq nc-tblank " "))
          (setq vtype (concat vtype tblank tword))
          (setq tblank " ")
          (setq tlist (cdr tlist))
          )
                                        ;      (message "shu-attr-name = \"%s\", shu-var-name = \"%s\", found-op = %s, vtype = \"%s\", shu-nc-vtype = \"%s\"" shu-attr-name shu-var-name found-op vtype shu-nc-vtype)
        (switch-to-buffer (get-buffer gs-buf))
        (goto-char (point-max))
        (cond
         ((string= found-op "&")
          (shu-return-ref)
          )
         ((string= found-op "*")
          (shu-return-ptr)
          (when (not shu-is-const-ptr)
            (shu-set-ptr))
          )
         (t ;; No operator at all
          (if is-base-type
              (progn
                (shu-emit-get)
                (insert (concat "  " shu-nc-vtype  "  " shu-var-name "() const\n"
                                "    {\n        return " shu-attr-name ";\n    }\n"))
                (shu-emit-set shu-var-name)
                (insert (concat
                         "  void set_" shu-var-name "(\n"
                         "    " vtype "   " shu-var-name ")\n"
                         "    {\n    "        shu-attr-name " = " shu-var-name ";\n    }\n"))
                ) ;; is-base-type
            ;; Not a base type, return a reference to it
            (shu-return-ref)
            (shu-set-obj)
            )
          )
         )
        (switch-to-buffer (get-buffer cbuf))
        )
      ) ; save-excursion
    ))

;;
;;  shu-return-ref  -  Return a reference to an instance variable
;;
(defun shu-return-ref ()
  (when (not shu-is-const)
    (shu-emit-get)
    (insert (concat "    " shu-nc-vtype "  &" shu-var-name "()\n"
                    "    {\n        return " shu-attr-name ";\n    }\n"))
    )
  (shu-emit-get)
  (insert (concat "    const " shu-nc-vtype "  &" shu-var-name "() const\n"
                  "    {\n        return " shu-attr-name ";\n    }\n"))
  )

;;
;;  shu-set-obj
;;
(defun shu-set-obj ()
  (shu-emit-set shu-var-name)
  (insert (concat
           "    void set_" shu-var-name "(\n"
           "        const " shu-nc-vtype "   &" shu-var-name ")\n"
           "    {\n    "        shu-attr-name " = " shu-var-name ";\n    }\n"))

  )

;;
;;  shu-return-ptr - Return a reference to something pointed to by an instance variable
;;                   This part emits the function declaration
;;
(defun shu-return-ptr ()
  (when (not shu-is-const)
    (shu-emit-get)
    (insert (concat "    " shu-nc-vtype "  &" shu-var-name "()\n"))
    (shu-gen-return-ptr)
    )
  (shu-emit-get)
  (insert (concat "    const " shu-nc-vtype "  &" shu-var-name "() const\n"))
  (shu-gen-return-ptr)
  )

;;
;;  shu-set-ptr
;;
(defun shu-set-ptr ()
  (shu-emit-set shu-var-name)
  (insert (concat
           "    void set_" shu-var-name "(\n"
           "        const " shu-nc-vtype "   &" shu-var-name ")\n"
           "    {\n    "        shu-attr-name " = &" shu-var-name ";\n    }\n"))

  )

;;
;;  shu-gen-return-ptr - Return a reference to something pointed to by an instance variable
;;                       This part emits the body of the function
;;
(defun shu-gen-return-ptr ()
  (insert (concat "    {\n"
                  "        assert(" shu-attr-name " != 0);\n"
                  "        return *" shu-attr-name ";\n"
                  "    }\n"))
  )

;;
;;  shu-emit-get
;;
(defun shu-emit-get ()
  (insert (concat
           "    \n\n"
           "    /*!\n"
           "     * \\brief Get " shu-lc-comment "\n"
           "     *\n"
           "     */\n"))
  )

;;
;;  shu-emit-set
;;
(defun shu-emit-set (arg)
  (insert (concat
           "    \n\n"
           "    /*!\n"
           "     * \\brief Set " shu-lc-comment "\n"
           "     *\n"
           "     */\n"))
  )


;;
;;  shu-csplit
;;
(defun shu-csplit (prefix)
  "Split a C++ string into multiple strings in order to keep the line length
below a certain minimum length..  The line length used is defined by the custom
variable SHU-CPP-LINE-END.

For example, you may copy a very long line of text into a section of code as
follows:

     static const std::string x(\"This is a very long line of text that looks as though it will go on forever.\");

To be polite to future code readers, you want to split this into multiple lines.
This can be a bit cumbersome if the text is very long.  This function splits the
text at a somewhat arbitrary boundary so that it can be read by others whose
text editors do not show code much beyond column 80 or so.  This is an example
of the above line after csplit was invoked:

     static const std::string x(\"This is a very long line of text that looks \"
                                \"as though it will go on forever.\");

This function normally splits lines on a word boundary.  If any prefix argument
is present, the split will be composed of fixed length lines with no respect to
word boundaries."
  (interactive "*P")
  (shu-internal-csplit prefix)
  )


;;
;;  shu-internal-csplit
;;
(defun shu-internal-csplit (&optional fixed-width)
  "This is the internal implementation of SHU-CSPLIT."
  (let ((xquote "[^\\]\"") ;; quote not preceded by escape
        (tstart (shu-point-in-string))
        (tend)
        (cc)
        (pad-count 0)
        (bpad "")
        (npad "")
        (line-limit 10)
        (original)
        (escape)
        (lines)
        (line)
        (nl ""))
    (if (not tstart)
        (progn
          (ding)
          (message "%s" "Not in string"))
      (goto-char tstart)
      (setq cc (current-column))
      (when (> cc 0)
        (setq pad-count (1- cc))
        (setq bpad (make-string pad-count ? )))
      (when (< pad-count shu-cpp-line-end)
        (setq line-limit (- shu-cpp-line-end pad-count 1))
        (when (< line-limit 10)
          (setq line-limit 10)))
      (setq line-limit (1- line-limit))
      (setq tend (re-search-forward xquote nil t))
      (if (not tend)
          (progn
            (ding)
            (message "%s" "No string end"))
        (setq original (buffer-substring-no-properties tstart (1- tend)))
        (when fixed-width
          (setq escape t))
        (setq lines (shu-misc-split-string original line-limit fixed-width escape))
        (goto-char (1- tstart))
        (delete-region (1- tstart) tend)
        (while lines
          (setq line (car lines))
          (insert (concat nl npad "\"" line "\""))
          (setq nl "\n")
          (setq npad bpad)
          (setq lines (cdr lines)))))
    ))



;;
;;  shu-cunsplit
;;
(defun shu-cunsplit ()
  "The beginnings of a re-write of SHU-CUNSPLIT.
Needs more testing.
Undo the split that was done by csplit.  Place the cursor anywhere
in any of the strings and invoke this function."
  (interactive)
  (let ((white-quote (concat shu-all-whitespace-regexp "*" "[^\\]\""))
        (x (shu-point-in-string))
        (going)
        (eos)       ;; End of last string
        (bos)       ;; Beginning of first string
        (cend)      ;; End of current string
        (del-count) ;; Number of characters to delete
        (pbegin)
        )   ;; Beginning of previous string
    (if (not x)
        (progn
          (ding)
          (message "%s" "Not in a string."))
      ;;; Search forward for last string in the group
      (setq going t)
      (while going
        (setq x (shu-end-of-dq-string))
        (if (not x)
            (setq going nil)
          (setq eos (- x 2))
          (skip-chars-forward shu-all-whitespace-regexp-scf)
          (if (not (looking-at "\""))
              (setq going nil)
            (forward-char 1)
            (setq x (shu-point-in-string))
            (when (not x)
              (setq going nil)))))
      ;;; End of last string
      (goto-char eos)
      (setq x (shu-point-in-string))
      (setq going t)
      ;;; Walk backwards deleting string separators
      (while going
        (setq bos x)
        (if (not (>= (- x 2) (point-min)))
            (setq going nil)
          (goto-char (- x 2))
          (setq pbegin (1- x))
          (skip-chars-backward shu-all-whitespace-regexp-scf)
          (if (not (> (point) (point-min)))
              (setq going nil)
            (when (not (looking-at "\""))
              (backward-char 1))
            (if (not (looking-at "\""))
                (setq going nil)
              (setq cend (point))
              (backward-char 1)
              (setq x (shu-point-in-string))
              (when (and pbegin cend)
                (setq del-count (- pbegin cend))
                (save-excursion
                  (goto-char cend)
                  (delete-char (1+ del-count))
                  (setq pbegin nil)
                  (setq cend nil)))
              (when (not x)
                (setq going nil)))))))
    ))




;;
;;  shu-creplace
;;
(defun shu-creplace (prefix)
  "This function will replace the C++ string in which point is placed with the
C++ string in the kill ring.  The C++ string in the kill ring is expected to be
a single string with or without quotes.  The C++ string in which point is placed
may have been split into smaller substrings in order to avoid long lines.

Assume you have the sample string that is shown in SHU-CSPLIT

     static const std::string x(\"This is a very long line of text that looks \"
                                \"as though it will go on forever.\");

You wish to replace it with a slightly different line of text, perhaps something
that came from the output of a program.  Copy the new string into the kill ring.
Then put the cursor into any part of any line of the string to be replaced
and invoke this function.  This function will remove the old string, replace it
with the contents of the string in the kill ring, and then split it up into
shorter lines as in the following example.  The string in the kill ring may have
opening and closing quotes or not.

     static const std::string x(\"This is a very long line of text that looks \"
                                \"as though it will go on forever and probably \"
                                \"already has done so or is threatening to do \"
                                \"so.\");

This is especially useful if you have a a string constant in a unit test and you
have modified the code that creates the string.  gtest will complain that the
expected string did not match the actual string.  If the actual string is
correct, copy it into the kill ring, go into your unit test, find the old
string, place the cursor in the old string, and replace it with the new."
  (interactive "*P")
  (shu-internal-creplace prefix)
  )



;;
;;  shu-internal-creplace
;;
(defun shu-internal-creplace (&optional fixed-width)
  "This is the internal implementation of SHU-CREPLACE."
  (let
      ((xquote "[^\\]\"") ;; quote not preceded by escape
       (start-quote-present)  ;; True if start quote in kill ring
       (end-quote-present)    ;; True if end quote in kill ring
       (have-quotes)          ;; True if kill ring string is quoted
       (unbalanced-quotes)    ;; True if kill ring quotes unbalanced
       (tstart)               ;; Start pos of quoted text in buffer
       (sos )                 ;; Start of string
       (eos )                 ;; End of string
       (del-count ))          ;; Count of chars we deleted in buffer
    (if (not kill-ring)
        (progn
          (ding)
          (message "%s" "Kill ring is empty"))
      ;;
      (with-temp-buffer
        (yank)
        (goto-char (point-min))
        (setq start-quote-present (looking-at "\""))
        (goto-char (1- (point-max)))
        (setq end-quote-present (looking-at "\"")))
      (when (or start-quote-present end-quote-present)
        ;; Have either start or end quote in kill ring
        (setq have-quotes t)
        (when (or (not start-quote-present) (not end-quote-present))
          ;; Missing either start or end quote in kill ring
          (setq unbalanced-quotes t)
          (when start-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at start but not at end"))
          (when end-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at end but not at start"))))
      (when (not unbalanced-quotes)
        ;; Find start of string in buffer
        (setq tstart (shu-point-in-string))
        (if (not tstart)
            ;; Point not positioned in a string in the buffer
            (progn
              (ding)
              (message "%s" "Not in string"))
          ;;
          (goto-char tstart) ; Position just after the quote
          (save-excursion  (shu-cunsplit)) ; Make it all one big string
          (setq sos (shu-point-in-string))
          (when have-quotes (setq sos (1- sos))) ;; Delete existing quote
          (re-search-forward xquote nil t)
          (forward-char -1) ;; Now sitting on top of closing quote
          (setq eos (point))
          (setq del-count (- eos sos))
          (when have-quotes (setq del-count (1+ del-count)))
          (goto-char sos)
          (delete-char del-count)
          (save-excursion (yank))
          (when have-quotes (forward-char 1))
          (shu-internal-csplit fixed-width))))
    ))



;;
;;  shu-s-mode-find-long-line
;;
(defun shu-s-mode-find-long-line ()
  "Place point in column 79 of the next line whose length exceeds 79 characters.
No movement occurs if no lines, starting with the current position, exceed 79
characters in length."
  (interactive)
  (let ((eol )        ;; point at end of current line
        (last-col )   ;; last column in current line
        (done )       ;; loop termination flag
        (start (point)) ;; Remember our start position
        (found1 )     ;; true if we find a long line
        (max-length 79))
    (while (not done)
      (setq last-col (save-excursion (end-of-line) (current-column)))
      (when (> last-col max-length)
        (setq found1 t)
        (setq done t)
        (beginning-of-line)
        (goto-char (1- (+ (point) max-length)))
        )
      (when (not done)
        (forward-line 1)
        (setq eol (save-excursion (end-of-line) (point)))
        (when (>= eol (point-max))
          (setq done t)))
      )
    (when (not found1)
      (message "%s" "No long lines")
      (goto-char start))
    ))


;;
;;  shu-cif
;;
(defun shu-cif ()
  "Insert an empty if statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "if ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 4))
    ))


;;
;;  shu-celse
;;
(defun shu-celse ()
  "Insert an empty else statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (insert (concat     "else\n"
                        pad "{\n"
                        pad "    "))
    (setq start (point))
    (insert "\n")
    (insert (concat pad "}\n"))
    (goto-char start)
    ))


;;
;;  shu-cfor
;;
(defun shu-cfor ()
  "Insert an empty for statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "for ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 5))
    ))


;;
;;  shu-cwhile
;;
(defun shu-cwhile ()
  "Insert an empty while statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "while ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 7))
    ))


;;
;;  shu-cdo
;;
(defun shu-cdo ()
  "Insert an empty do statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "do\n"
                        pad "{\n"))
    (setq start (1- (point)))
    (insert (concat  pad "} while();"))
    (goto-char (+ start pad-count 9))
    ))



;;
;;  shu-new-deallocate
;;
(defun shu-new-deallocate (var-name)
  "Insert the code to do a standard deallocation of memory allocated by a
specific allocator.  First prompt reads the variable name that points to the
memory to be deallocated.  Second prompt reads the name of the class whose
destructor is to be called.

This generates a code sequence as follows:

        if (var-name)
        {
            m_allocator->deleteObject(var-name);
            var-name = 0;
        }

If SHU-CPP-MODERN is true, the code sequence is:

        if (var-name != nullptr)
        {
            m_allocator->deleteObject(var-name);
            var-name = nullptr;
        }

VAR-NAME is read from a prompt.  The number of spaces to indent inside that
braces is defined in the custom variable shu-cpp-indent-length.  The name of the
member variable that points to the allocator in use by the class comes from the
custom variable shu-cpp-default-allocator-name"
  (interactive "*sVariable name?: ")
  (let ((pad)
        (pad-count (current-column))
        (start)
        (ipad (make-string shu-cpp-indent-length ? ))
        (equal-test (if shu-cpp-modern " != nullptr" ""))
        (null-value (if shu-cpp-modern "nullptr" "0")))
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "if (" var-name equal-test ")\n"
      pad "{\n"
      pad ipad shu-cpp-default-allocator-name "->deleteObject(" var-name ");\n"
      pad ipad var-name " = " null-value ";\n"
      pad "}\n"))
    ))



;;
;;  shu-citerate
;;
(defun shu-citerate (type-name var-name)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the type name.  Second
prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length."
  (interactive "*sType name?: \nsVariable name?: ")
  (shu-internal-citerate type-name var-name)
  )



;;
;;  shu-cciterate
;;
(defun shu-cciterate (type-name var-name)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the type name.  Second
prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::const_iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length."
  (interactive "*sType name?: \nsVariable name?: ")
  (shu-internal-citerate type-name var-name t)
  )



;;
;;  shu-internal-citerate
;;
(defun shu-internal-citerate (type-name var-name &optional const)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the variable name.
Second prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

If optional CONST is true, a const iterator is generated."
  (let ((const-qual (if const "const_" ""))
        (rpoint)
        (pad)
        (pad-count (current-column))
        (start)
        (ipad (make-string shu-cpp-indent-length ? )))
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "for (" type-name "::" const-qual "iterator it = " var-name ".begin();\n"
      pad
      "     it != " var-name  ".end(); ++it)\n"
      pad "{\n"
      pad ipad))
    (setq rpoint (point))
    (insert
     (concat
      "\n"
      pad "}\n"))
    (goto-char rpoint)
    ))




;;
;;  shu-diterate
;;
(defun shu-diterate (type-name var-name-1 var-name-2)
  "Insert the code to iterate through a pair of data structures of type
TYPE-NAME, whose first instance is identified by VAR-NAME-1 and whose second
instance is identified by VAR-NAME-2.

The first prompt reads the type name, second and third prompts read the two
variable names.

The generated code sequence is as follows:

      for (std::pair<type-name::iterator,
                     type-name::iterator>
               its(var-name-1.begin(), var-name-2.begin());
           its.first != var-name-1.end() && its.second != var-name-2.end();
           ++its.first, ++its.second)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length.

The name of the namespace used for the standard library is defined in the custom
variable shu-cpp-std-namespace."
  (interactive "*sType name?: \nsFirst variable name?:  \nsSecond variable name?: ")
  (shu-internal-double-citerate type-name type-name var-name-1 var-name-2)
  )




;;
;;  shu-dciterate
;;
(defun shu-dciterate (type-name var-name-1 var-name-2)
  "Insert the code to iterate through a pair of data structures of type
TYPE-NAME, whose first instance is identified by VAR-NAME-1 and whose second
instance is identified by VAR-NAME-2.

The first prompt reads the type name, second and third prompts read the two
variable names.

The generated code sequence is as follows:

      for (std::pair<type-name::const_iterator,
                     type-name::const_iterator>
               its(var-name-1.begin(), var-name-2.begin());
           its.first != var-name-1.end() && its.second != var-name-2.end();
           ++its.first, ++its.second)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length.

The name of the namespace used for the standard library is defined in the custom
variable shu-cpp-std-namespace."
  (interactive "*sType name?: \nsFirst variable name?:  \nsSecond variable name?: ")
  (shu-internal-double-citerate type-name type-name var-name-1 var-name-2 t)
  )




;;
;;  shu-titerate
;;
(defun shu-titerate (type-name-1 type-name-2 var-name-1 var-name-2)
  "Insert the code to iterate through a pair of data structures of types
TYPE-NAME-1 and TYPE-NAME-2, whose first instance is identified by VAR-NAME-1
and whose second instance is identified by VAR-NAME-2.

The first two prompt reads the two type names, third and fourth prompts read the
two variable names.

The generated code sequence is as follows:

      for (std::pair<type-name-1::iterator,
                     type-name-2::iterator>
               its(var-name-1.begin(), var-name-2.begin());
           its.first != var-name-1.end() && its.second != var-name-2.end();
           ++its.first, ++its.second)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length.

The name of the namespace used for the standard library is defined in the custom
variable shu-cpp-std-namespace."
  (interactive "*sFirst type name?: \nsSecond type name?: \nsFirst variable name?:  \nsSecond variable name?: ")
  (shu-internal-double-citerate type-name-1 type-name-2 var-name-1 var-name-2)
  )




;;
;;  shu-tciterate
;;
(defun shu-tciterate (type-name-1 type-name-2 var-name-1 var-name-2)
  "Insert the code to iterate through a pair of data structures of types
TYPE-NAME-1 and TYPE-NAME-2, whose first instance is identified by VAR-NAME-1
and whose second instance is identified by VAR-NAME-2.

The first two prompt reads the two type names, third and fourth prompts read the
two variable names.

The generated code sequence is as follows:

      for (std::pair<type-name-1::const_iterator,
                     type-name-2::const_iterator>
               its(var-name-1.begin(), var-name-2.begin());
           its.first != var-name-1.end() && its.second != var-name-2.end();
           ++its.first, ++its.second)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length.

The name of the namespace used for the standard library is defined in the custom
variable shu-cpp-std-namespace."
  (interactive "*sFirst type name?: \nsSecond type name?: \nsFirst variable name?:  \nsSecond variable name?: ")
  (shu-internal-double-citerate type-name-1 type-name-2 var-name-1 var-name-2 t)
  )



;;
;;  shu-internal-double-citerate
;;
(defun shu-internal-double-citerate (type-name-1 type-name-2 var-name-1 var-name-2 &optional const)
  "Insert the code to iterate through a pair of data structures of types
TYPE-NAME-1 and TYPE-NAME-2, whose first instance is identified by VAR-NAME-1
and whose second instance is identified by VAR-NAME-2.

The generated code sequence is as follows:

      for (std::pair<type-name-1::const_iterator,
                     type-name-2::const_iterator>
               its(var-name-1.begin(), var-name-2.begin());
           its.first != var-name-1.end() && its.second != var-name-2.end();
           ++its.first, ++its.second)
      {
      }

The number of spaces to indent inside the braces is defined in the custom
variable shu-cpp-indent-length.

The name of the namespace used for the standard library is defined in the custom
variable shu-cpp-std-namespace.

If optional CONST is true, a const iterator is generated."
  (let ((const-qual (if const "const_" ""))
        (rpoint)
        (pad)
        (pad-count (current-column))
        (start)
        (ipad (make-string shu-cpp-indent-length ? ))
        (std-name shu-cpp-std-namespace)
        (pair-name))
    (setq pair-name (concat std-name "::pair"))
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "for (" pair-name "<" type-name-1 "::" const-qual "iterator,\n"
      pad
      "               "  type-name-2 "::" const-qual "iterator>\n"
      pad
      "         its(" var-name-1 ".begin(), " var-name-2 ".begin());\n"
      pad
      "     its.first != " var-name-1 ".end() && its.second != " var-name-2 ".end();\n"
      pad
      "     ++its.first, ++its.second)\n"
      pad "{\n"
      pad ipad))
    (setq rpoint (point))
    (insert
     (concat "\n"
             pad "}\n"))
    (goto-char rpoint)
    ))



;;
;;  TODO: Any bunch of stuff that appears between << operators must have
;;        balanced parenthesis.
;;

;;
;;  shu-cpp-check-streaming-op
;;
(defun shu-cpp-check-streaming-op (start end)
  "Check a streaming operation.   Mark a region that contains a set of streaming
operators and invoke this function.  It will make sure that you have no unterminated
strings and that you are not missing any occurrences of <<."
  (interactive "r")                 ; bounds of marked region (start end)
  (let (
        (ret-val)
        (token-list)
        (error-token-info)
        (info)
        (token)
        (token-type)
        (ext-info)
        (point-pair)
        (error-message)
        (tbuf      (get-buffer-create shu-unit-test-buffer))
        (emsg)
        (spoint)
        (debug-on-error t)
        (epoint)
        )
    (setq ret-val (shu-cpp-tokenize-region start end))
    (setq token-list (cdr ret-val))
    (shu-cpp-tokenize-show-list token-list)

    (setq error-token-info (car ret-val))
    (when (not error-token-info)
      (setq error-token-info (shu-cpp-internal-stream-check token-list))
      )
    (if (not error-token-info)
        (message "%s" "OK")
      (shu-cpp-token-show-token-info error-token-info)
      (setq info (car error-token-info))
      (setq token (cdr error-token-info))
      (setq token-type (car info))
      (setq ext-info (cdr info))
      (setq error-message (car ext-info))
      (setq point-pair (cdr ext-info))
      (setq spoint (car point-pair))
      (setq epoint (cdr point-pair))
      (setq emsg error-message)
      (when (not error-message)
        (setq emsg "Uknown error at point"))
      (goto-char spoint)
      (message "%s" emsg)
      )
    ))


;;
;;  shu-cpp-internal-stream-check
;;
(defun shu-cpp-internal-stream-check (token-list)
  "Take a list of tokens found in a C++ streaming operation and check to
ensure that every other token is a << operator.  Two adjacent occurrences of <<
represent an extraneous << operator.  Two adjacent occurrences of tokens that
are not << represent a missing << operator."
  (let
      ((tlist token-list)
       (token-info)
       (token)
       (last-op-token "")
       (expecting-op)
       (done)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (info)
       (error-message)
       (token)
       (ext-info)
       (token-type)
       (point-pair)
       (spoint)
       (epoint)
       (ok-op)
       (debug-on-error t)
       (error-token-info))
    (setq token-info (car tlist))
    (setq info (car token-info))
    (setq token (cdr token-info))
    (setq token-type (car info))
    (setq expecting-op nil)
    (when (= token-type shu-cpp-token-type-op)
      (setq expecting-op t))
    (setq done nil)
    (while (not done)
      (setq token-info (car tlist))
      (setq info (car token-info))
      (setq token (cdr token-info))
      (setq token-type (car info))
      (when (/= token-type shu-cpp-token-type-ct) ;; Ignore comments
        (setq ext-info (cdr info))
        (setq point-pair (cdr ext-info))
        (setq spoint (car point-pair))
        (setq epoint (cdr point-pair))
        (if expecting-op
            ;; Expecting an operator and we found one
            (if (= token-type shu-cpp-token-type-op)
                (setq expecting-op nil) ; No longer expecting an op
              ;; Expecting an operator and we did not find one
              (setq error-message "Missing << at point")
              (setq error-token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
              (setq done t))
          ;; Not expecting an operator and we did not find one
          (if (/= token-type shu-cpp-token-type-op)
              (setq expecting-op t) ; Now we are expecting an operator
            ;; Not expecting an operator and we did find one
            (setq ok-op (or (shu-cpp-is-enclosing-op last-op-token) (string= token "(")))
            (when (not ok-op)
              (setq error-message "Extraneous << at point")
              (setq error-token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
              (setq done t))))
        (when (= token-type shu-cpp-token-type-op)
          (setq last-op-token token)))
      (setq tlist (cdr tlist))
      (when (not tlist)
        (setq done t)))
    error-token-info
    ))


(defun shu-cpp-is-enclosing-op (op)
  "Return true if the single character in OP is an enclosing character, a left
or right parenthesis or a left or right square bracket."
  (let ((is-enc))
    (setq is-enc (or
                  (string= op ")")
                  (string= op "(")
                  (string= op "]")
                  (string= op "[")))
    is-enc
    ))






;;
;;  shu-cpp-rmv-using-old
;;
;;  Deprecated:  See the new version of shu-cpp-rmv-using in shu-match.el
;;
(defun shu-cpp-rmv-using-old (class-list &optional top-name)
  "Remove \"using namespace\" directives from a C++ file, adding the appropriate
namespace qualifier to all of the unqualified class names.  CLASS-LIST is an
a-list in which the car of each entry is a namespace and the cdr of each entry
is a list of class names.  Here is an example of such an a-list:

     (list
      (cons \"std\"    (list \"set\" \"string\" \"vector\"))
      (cons \"world\"  (list \"Hello\" \"Goodbye\")))

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;

NB: This version is deprecated.  See the new version in shu-match.el"
  (let* ((gb-name "**shu-chgs**")
         (gb (get-buffer-create gb-name))
         (ct 0)
         (count 0)
         (uc 0)
         (unk "")
         (looking)
         (name)
         (mbeg)
         (item)
         (added-item)
         (duplicates)
         (x)
         (classes)
         (namespace)
         (case-fold-search nil))
    (if (shu-cpp-rmv-blocked class-list top-name gb)
        (progn
          (ding)
          (message "Class ambiguity prevents change.  See buffer %s" gb-name))
      (goto-char (point-min))
      (setq looking t)
      (while looking
        (setq name (shu-cpp-find-using top-name))
        (if (not name)
            (setq looking nil)
          (setq mbeg (match-beginning 0))
          (setq item (cons name (line-number-at-pos mbeg)))
          (shu-add-to-alist added-item item duplicates)
          (if (not (eq added-item item)) ;; Name is a duplicate
              (delete-region (line-beginning-position) (line-end-position))
            (setq x (assoc name class-list))
            (if (not x)
                (progn
                  (princ (format "Unknown namespace: \"%s\"\n" name) gb)
                  (setq uc (1+ uc)))
              (delete-region (line-beginning-position) (line-end-position))
              (setq namespace (car x))
              (setq classes (cdr x))
              (save-excursion
                (setq ct (shu-cpp-qualify-classes classes namespace gb)))
              (setq count (+ count ct))))))
      (goto-char (point-min))
      (when (not (= 0 uc))
        (setq unk (format " %d unknown namespaces. " uc)))
      (message "Replaced %d occurrences.%s  See buffer %s" count unk gb-name))
    count
    ))



;;
;;  shu-cpp-rmv-blocked
;;
(defun shu-cpp-rmv-blocked (class-list top-name gb)
  "Do a pre-check on a file to see if we will be able to remove its \"using
namespace\" directives.  CLASS-LIST is the a-list passed to SHU-CPP-RMV-USING.
USING is the regular expression used to search for \"using namespace\"
directives.  TOP-QUAL is the regular expression used to strip out a higher level
qualifier from the class name in a \"using namespace\" directive, if any.  GB is
the buffer into which diagnostic messages are written.

This function finds all of the \"using namespace\" directives in the file and
checks to see if there is any ambiguity in the resulting class list.  For
example, if namespace \"mumble\" contains class \"Bumble\" and namespace
\"stubble\" also contains class \"Bumble\", we will not know which namespace to
apply to instances of class \"Bumble\".  But this is not an ambiguity if there
is a \"using namespace\" directive for only one of those classes.  That is why
we do the ambiguity check only for namespaces referenced by \"using namespace\"
directives.

This function returns true if such an ambiguity exists."
  (let ((name)
        (mbeg)
        (bol)
        (x)
        (z)
        (uc 0)
        (looking)
        (item)
        (added-item)
        (duplicates)
        (clist)
        (cl)
        (ns)
        (classes)
        (class)
        (listc)
        (blocked))
    (save-excursion
      (goto-char (point-min))
      (setq looking t)
      (while looking
        (setq name (shu-cpp-find-using top-name))
        (if (not name)
            (setq looking nil)
          (setq mbeg (match-beginning 0))
          (setq item (cons name (line-number-at-pos mbeg)))
          (shu-add-to-alist added-item item duplicates)
          (when (eq added-item item) ;; Name is not duplicate
            (setq x (assoc name class-list))
            (when x
              (setq clist (cons x clist)))))))
    (setq cl clist)
    (while cl
      (setq x (car cl))
      (setq ns (car x))
      (setq classes (cdr x))
      (while classes
        (setq class (car classes))
        (setq x (cons class ns))
        (if (not listc)
            (setq listc (cons x listc))
          (setq z (assoc class listc))
          (if (not z)
              (setq listc (cons x listc))
            (princ (format "class %s in namespace %s conflicts with class %s in namespace %s\n"
                           class ns (car z) (cdr z)) gb)
            (setq blocked t)))
        (setq classes (cdr classes)))
      (setq cl (cdr cl)))
    blocked
    ))



;;
;;  shu-cpp-find-using
;;
(defun shu-cpp-find-using (&optional top-name)
  "Return the name of the class found on the next \"using namespace\" directive
or nil of no such directive found.

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (interactive)
  (let ((using "using\\s-+namespace\\s-+\\([a-zA-Z0-9:_$]+\\)\\s-*;")
        (looking t)
        (top-qual (when top-name (concat top-name "::\\([a-zA-Z0-9_$]+\\)")))
        (name)
        (using-name)
        (mbeg)
        (bol)
        (not-comment))
    (while looking
      (setq using-name nil)
      (setq not-comment nil)
      (if (not (re-search-forward using nil t))
          (setq looking nil)
        (setq name (match-string 1))
        (setq mbeg (match-beginning 0))
        (setq bol (line-beginning-position))
        (save-match-data
          (save-excursion
            (when (not (shu-point-in-string (1- (point))))
              (setq not-comment t)
              (goto-char bol)
              (when (search-forward "//" mbeg t)
                (setq not-comment nil)))
            (when not-comment
              (when top-qual
                (when (string-match top-qual name)
                  (setq name (match-string 1 name)))))
            (when not-comment
              (setq using-name name)
              (setq looking nil))))))
    using-name
    ))




;;
;;  shu-qualify-class-name
;;
(defun shu-qualify-class-name (target-name namespace)
  "Find all instances of the class name TARGET-NAME and add an explicit namespace
qualifier NAMESPACE.  If the TARGET-NAME is \"Mumble\" and the NAMESPACE is
\"abcd\", then \"Mumble\" becomes \"abcd::Mumble\".  But variable names such
as \"d_Mumble\" or \"MumbleIn\" remain unchanged and already qualified class
names remain unchanged.
This is intended to help rescue code that has one or more \"using namespace\"
directives in it.  The problem with \"using namespace\" is that you now have
class names from other namespaces with no easy way to identify the namespace
to which they belong.  The best thing to do is get rid of the \"using
namespace\" statements and explicitly qualify the class names.  But if you
use a simple replace to do that, you will qualify variable names that resemble
class names as well as class names that are already qualified.  This function
only adds a namespace to a class name that does not already have a namespace
qualifier."
  (shu-internal-replace-class-name target-name 'shu-qualify-class-fun namespace)
  )



;;
;;  shu-qualify-class-fun
;;
(defun shu-qualify-class-fun (namespace name)
  "This is the replacement function for SHU-QUALIFY-CLASS-NAME.  It is called
with the NAMESPACE to be applied to the class whose name is NAME.  It
constructs a new name and issues replace-match to replace it."
  (let ((rename (concat namespace "::" name)))
    (replace-match rename t t)
    ))



;;
;;  shu-replace-class-name
;;
(defun shu-replace-class-name (target-name new-name &optional in-string in-comment)
  "Find all instances of the class name TARGET-NAME and replace it with the name
NEW-NAME.  If the target name is \"Mumble\", then all instances of \"Mumble\"
that resemble class names are replaced.  But names such as \"d_Mumble\" or
\"MumbleIn\" remain unchanged.  if IN-STRING is true, then instances of the
class name found inside a string are replaced.  if IN-COMMENT is true, then
instances of the class name found inside a comment are replaced."
  (shu-internal-replace-class-name target-name 'shu-replace-class-fun
                                   new-name in-string in-comment)
  )



;;
;;  shu-replace-class-fun
;;
(defun shu-replace-class-fun (new-name name)
  "This is the replacement function for SHU-REPLACE-CLASS-NAME.  It is called
with the NEW-NAME to replace the class NAME.  It calls replace-match to replace
NAME with NEW-NAME."
  (replace-match new-name t t)
  )



;;
;;  shu-internal-replace-class-name
;;
(defun shu-internal-replace-class-name (target-name replace-func replace-arg
                                                    &optional in-string in-comment)
  "Find all instances of the class name TARGET-NAME and if it actually appears
to be a class name, call REPLACE-FUN passing to it REPLACE-ARG and the class
name.  REPLACE-FUN issues the appropriate replace-match call, constructing the
replacement for the class name from some combination of REPLACE-ARG and the
class name.  IN-STRING is true if a class name inside of a string is to be
replaced.  IN-COMMENT is true if a class name inside of a comment is to be
replaced."
  (let ((bol)
        (eol)
        (mbeg)
        (mend)
        (have-match)
        (name)
        (rename)
        (count 0)
        (prefix-rx "[:>.]")
        (case-fold-search nil)
        )
    (while (search-forward target-name nil t)
      (setq bol (line-beginning-position))
      (setq eol (line-end-position))
      (setq name (match-string 0))
      (setq mbeg (match-beginning 0))
      (setq mend (match-end 0))
      (setq have-match t)
      (save-match-data
        (when (> mbeg bol)
          (save-excursion
            (if (shu-class-is-blocked mbeg in-string in-comment)
                (setq have-match nil)
              (goto-char (1- mbeg))
              (if (looking-at shu-cpp-name)
                  (setq have-match nil)
                (save-match-data
                  (if (looking-at shu-not-all-whitespace-regexp)
                      (progn
                        (when (looking-at prefix-rx)
                          (setq have-match nil)))
                    (when (re-search-backward shu-not-all-whitespace-regexp bol t)
                      (when (looking-at prefix-rx)
                        (setq have-match nil))))))))))
      (when have-match
        (when (< mend eol)
          (save-excursion
            (goto-char mend)
            (when (looking-at shu-cpp-name)
              (setq have-match nil)))))
      (when have-match
        (save-excursion
          (save-match-data
            (goto-char bol)
            (when (re-search-forward "#\\s-*include" mbeg t)
              (setq have-match nil)))))
      (when have-match
        (funcall replace-func replace-arg name)
        (setq count (1+ count))))
    count
    ))



;;
;;  shu-class-is-blocked
;;
(defun shu-class-is-blocked (pos  &optional in-string in-comment)
  "Return true if a class name should be ignored because it is either in a
string or a comment.

We have found something at point POS that looks as though it might be a class
name.  If it is in a string or is preceded on the same line by \"//\" (also not
in a string), then it is either in a string or is probably in a comment, so we
may want to ignore it.  IN-STRING is true if a class name inside of a string is
to be replaced.  IN-COMMENT is true if a class name inside of a comment is to be
replaced.

Return true if the class name should be ignored."
  (let ((bol (line-beginning-position))
        (no-string (not in-string))
        (no-comment (not in-comment))
        (blocked))
    (save-excursion
      (if (and no-string
               (shu-point-in-string pos))
          (setq blocked t)
        (goto-char bol)
        (when no-comment
          (when (search-forward "//" pos t)
            (when (not (shu-point-in-string))
              (setq blocked t))))))
    blocked
    ))




;;
;;  shu-interactive-qualify-class-name
;;
(defun shu-interactive-qualify-class-name ()
  "Interactively call SHU-QUALIFY-CLASS-NAME to find all instances of a class name and
add a namespace qualifier to it.  First prompt is for the class name.  If a fully qualified
class name is supplied, then the given namespace is applied to the class name.  If the name
supplied is not a namespace qualified class name, then a second prompt is given to read the
namespace.
This is intended to help rescue code that has one or more \"using namespace\"
directives in it.  The problem with \"using namespace\" is that you now have
class names from other namespaces with no easy way to identify the namespace
to which they belong.  The best thing to do is get rid of the \"using
namespace\" statements and explicitly qualify the class names.  But if you
use a simple replace to do that, you will qualify variable names that resemble
class names as well as class names that are already qualified.  This function
only adds a namespace to a class name that does not already have a namespace
qualifier."
  (interactive)
  (let ((class "")
        (qclass "")
        (qual "")
        (sqr (concat "\\(" shu-cpp-name "+\\)\\s-*::\\s-*\\(" shu-cpp-name "+\\)"))
        (count 0)
        (case-fold-search nil)
        (minibuffer-allow-text-properties nil))
    (while (= 0 (length qclass))
      (setq qclass (read-from-minibuffer "Class name? ")))
    (setq class qclass)
    (when (string-match sqr qclass)
      (setq qual (match-string 1 qclass))
      (setq class (match-string 2 qclass)))
    (while (= 0 (length qual))
      (setq qual (read-from-minibuffer "namespace? ")))
    (setq count (shu-qualify-class-name class qual))
    (message "Replaced %d occurrences" count)
    ))



;;
;;  shu-cpp-qualify-classes
;;
(defun shu-cpp-qualify-classes (class-list namespace &optional buffer)
  "Repeatedly call SHU-QUALIFY-CLASS-NAME for all class names in CLASS-LIST.
NAMESPACE is either the name of a single namespace to apply to all classes
in CLASS-LIST or is a list of namespaces each of which has a one to one
correspondence with a class name in CLASS-LIST.  The optional BUFFER
argument may be a buffer in which the actions are recorded.  Return the
number of names changed."
  (let ((classes class-list)
        (names namespace)
        (buf-msg "")
        (class-name)
        (cnt)
        (count 0)
        (ct)
        (ns))
    (when buffer
      (when (not (bufferp buffer))
        (error "Supplied BUFFER argument is not a buffer")))
    (cond
     ((stringp names)
      (setq ns names)
      (while classes
        (setq class-name (car classes))
        (goto-char (point-min))
        (setq ct (shu-qualify-class-name class-name ns))
        (setq count (+ count ct))
        (setq cnt (shu-fixed-format-num ct 8))
        (when buffer
          (princ (format "%s: %s::%s\n" cnt ns class-name) buffer))
        (setq classes (cdr classes))))
     ((listp names)
      (when (not (= (length classes) (length names)))
        (error "Length of CLASS-LIST list (%d) not same as length of NAMESPACE list (%d)"
               (length classes) (length names)))
      (while classes
        (setq class-name (car classes))
        (setq ns (car names))
        (goto-char (point-min))
        (setq ct (shu-qualify-class-name class-name ns))
        (setq count (+ count ct))
        (setq cnt (shu-fixed-format-num ct 8))
        (when buffer
          (princ (format "%s: %s::%s\n" cnt ns class-name) buffer)
          (setq names (cdr names)))
        (setq classes (cdr classes))))
     (t
      (error "NAMES argument is neither a string nor a list")))
    (goto-char (point-min))
    (when buffer
      (setq buf-msg (concat "  See buffer " (buffer-name buffer))))
    (message "Replaced %d occurrences.%s" count buf-msg)
    count
    ))



;;
;;  shu-qualify-namespace-std
;;
(defun shu-qualify-namespace-std ()
  "Add \"std\" namespace qualifier to some of the classes in \"std\".  Return the
count of class names changed."
  (interactive)
  (let ((gb (get-buffer-create "**chgs**"))
        (namespace "std")
        (classes (list
                  "endl"
                  "ifstream"
                  "ios_base"
                  "map"
                  "ostringstream"
                  "pair"
                  "set"
                  "setfill"
                  "setw"
                  "string"
                  "vector"))
        (count 0))
    (setq count (shu-cpp-qualify-classes classes namespace gb))
    count
    ))



;;
;;  shu-qualify-namespace-bsl
;;
(defun shu-qualify-namespace-bsl ()
  "Add \"bsl\" namespace qualifier to some of the classes in \"bsl\".  Return the
count of class names changed."
  (interactive)
  (let ((gb (get-buffer-create "**chgs**"))
        (namespace "bsl")
        (classes (list
                  "endl"
                  "ifstream"
                  "ios_base"
                  "map"
                  "ostringstream"
                  "pair"
                  "set"
                  "setfill"
                  "setw"
                  "string"
                  "vector"))
        (count 0))
    (setq count (shu-cpp-qualify-classes classes namespace gb))
    count
    ))



;;
;;  shu-dbx-summarize-malloc
;;
(defun shu-dbx-summarize-malloc ()
  "Go through the output of a dbx malloc dump and generate a summary.  dbx is
the AIX debugger.  It has a malloc command that goes through the heap and prints
one line for every allocated buffer.  Here is a sample of some of its output:

         ADDRESS         SIZE HEAP    ALLOCATOR
      0x30635678          680    0     YORKTOWN
      0x30635928          680    0     YORKTOWN
      0x30635bd8          680    0    HEAPCACHE
      0x30635bcf          680    0     YORKTOWN

YORKTOWN is the name of the default allocator on AIX.  This function goes
through the malloc output and gets the number and sizes of all buffers
allocated.  This tells you how many buffers were allocated, the total number of
bytes allocated, and the total number of buffers allocated by size.  The output
is placed in a separate buffer called **shu-aix-malloc**."
  (interactive)
  (let ((gb (get-buffer-create "**shu-aix-malloc**"))
        (rs   "0x\\([a-f0-9]+\\)\\s-+\\([0-9]+\\)\\s-+[0-9]+\\s-+\\([A-Z]+\\)")
        (address 0)
        (size 0)
        (sizes)
        (count 0)
        (x)
        (z)
        (buf-count 0)
        (allocator)
        (line-no 0)
        (pline)
        (ht (make-hash-table :test 'equal :size 5000))
        (aht (make-hash-table :test 'equal :size 50))
        (start-time (current-time))
        (tformat "%M:%S.%3N")
        (elapsed)
        (ptime))
    (setq debug-on-error t)
    (goto-char (point-min))
    (while (re-search-forward rs nil t)
      (setq address (match-string 1))
      (setq size (match-string 2))
      (setq allocator (match-string 3))
      (setq size (string-to-number size))
      (setq count (gethash size ht))
      (if count
          (setq count (1+ count))
        (setq count 1))
      (puthash size count ht)
      (setq count (gethash allocator aht))
      (if count
          (setq count (1+ count))
        (setq count 1))
      (puthash allocator count aht)
      (setq line-no (1+ line-no))
      (when (= 0 (% line-no 100000))
        (setq elapsed (time-subtract (current-time) start-time))
        (setq ptime (format-time-string tformat elapsed))
        (setq pline (shu-group-number line-no))
        (message "Line: %s  Elapsed: %s" pline ptime)))
    (setq elapsed (time-subtract (current-time) start-time))
    (setq ptime (format-time-string tformat elapsed))
    (setq pline (shu-group-number line-no))
    (message "Line: %s  Elapsed: %s  **done**" pline ptime)
    (setq sizes nil)
    (maphash (lambda (size count)
               (push (cons size count) sizes)) ht)
    (setq sizes (sort sizes (lambda(lhs rhs) (< (car lhs) (car rhs)))))
    (shu-aix-show-malloc-list sizes gb)
    (setq sizes (sort sizes (lambda(lhs rhs) (< (cdr lhs) (cdr rhs)))))
    (shu-aix-show-malloc-list sizes gb)
    (setq sizes (sort sizes (lambda(lhs rhs) (< (* (car lhs) (cdr lhs))(* (car rhs) (cdr rhs))))))
    (shu-aix-show-malloc-list sizes gb)
    (setq sizes nil)
    (maphash (lambda (allocator count)
               (push (cons allocator count) sizes)) aht)
    (setq sizes (sort sizes (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (shu-aix-show-allocators sizes gb)
    ))



;;
;;  shu-aix-show-allocators
;;
(defun shu-aix-show-allocators (sizes gb)
  "SIZES is an alist whose car is an allocator name and whose cdr is the number of
allocations attributed to that allocator.  For each allocator, display in the
buffer GB, the name of the allocator and its counts"
  (let ((sz sizes)
        (cp)
        (max-len 0)
        (allocator)
        (count)
        (pcount)
        (pad)
        (pad-len))
    (princ "\n\nAllocation counts by allocator:\n" gb)
    (while sz
      (setq cp (car sz))
      (setq allocator (car cp))
      (when (> (length allocator) max-len)
        (setq max-len (length allocator)))
      (setq sz (cdr sz)))
    (setq sz sizes)
    (while sz
      (setq cp (car sz))
      (setq allocator (car cp))
      (setq count (cdr cp))
      (setq pad "")
      (when (< (length allocator) max-len)
        (setq pad-len (- max-len (length allocator)))
        (setq pad (make-string pad-len ? )))
      (setq pcount (shu-fixed-format-num count 14))
      (princ (concat allocator ":" pad pcount "\n") gb)
      (setq sz (cdr sz)))
    ))



;;
;;  shu-aix-show-malloc-list
;;
(defun shu-aix-show-malloc-list (mlist gb)
  "Print the number of buffers allocated by size from an AIX dbx malloc command."
  (let ((x mlist)
        (z)
        (size)
        (count 0)
        (total 0)
        (this 0)
        (nthis)
        (nsize)
        (ncount)
        (buf-count 0)
        (ntotal)
        (nbuf-count))
    (princ "\n\n" gb)
    (princ "      buffer    buffer\n" gb)
    (princ "        size     count\n" gb)
    (while x
      (setq z (car x))
      (setq size (car z))
      (setq count (cdr z))
      (setq this (* size count))
      (setq total (+ total this))
      (setq nsize (shu-fixed-format-num size 12))
      (setq nthis (shu-fixed-format-num this 12))
      (setq ncount (shu-fixed-format-num count 8))
      (princ (concat nsize ": " ncount "   (" nthis ")" "\n") gb)
      (setq buf-count (+ count buf-count))
      (setq x (cdr x)))
    (setq ntotal (shu-group-number total))
    (setq nbuf-count (shu-group-number buf-count))
    (princ (format "\nbuf-count: %s buffers allocated\n" nbuf-count) gb)
    (princ (format "total: %s bytes allocated\n" ntotal) gb)
    (princ (format "%s distinct buffer sizes found\n" (shu-group-number (length mlist))) gb)
    (cons total buf-count)
    ))



;;
;;  shu-cpp-fix-prototype
;;
(defun shu-cpp-fix-prototype ()
  "Place the cursor on the beginning of a function declaration that has been
copied from a .cpp file to a .h file.  This function fixes up the function
prototype to make it suitable for a .h file.
For example, this declaration:

      double Frobnitz::hitRatio(
          const int  reads,
          const int  writes)
      const

would be transformed into

          double hitRatio(
              const int  reads,
              const int  writes)
          const;"
  (interactive)
  (let ((all-white (concat shu-not-all-whitespace-regexp "*"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (eos (+ 10 (line-end-position)))
        (bon)
        (eon)
        (ns-length)
        (limit))
    (if (not (re-search-forward all-white eol t))
        (progn
          (ding)
          (message "%s" "No whitespace found to start namespace qualifier"))
      (setq bon (1+ (point)))
      (if (not (search-forward "::" eol t))
          (progn
            (ding)
            (message "%s" "Cannot find \"::\" namespace separator"))
        (setq eon (point))
        (setq ns-length (- eon bon))
        (if (not (search-forward "(" eos t))
            (progn
              (ding)
              (message "%s" "Cannot find opening left parenthesis"))
          (backward-char 1)
          (forward-sexp)
          (setq eos (point))
          (setq limit (+ 11 eos))
          (when (search-forward "const" limit t)
            (setq eos (point)))
          (goto-char eos)
          (insert ";")
          (setq eos (- eos ns-length))
          (delete-region bon eon)
          (shu-shift-region-of-text shu-cpp-indent-length bol eos))))
    ))


;;
;;  shu-simple-hother-file
;;
(defun shu-simple-hother-file ()
  "Return the name of the .h file that corresponds to the .cpp file or .t.cpp file
that is in the current buffer.  This version of the function creates the name of
the .h file from the name of the file in the current buffer.  This is in contrast
with the function shu-hother which finds the corresponding .h file from the list
of files in the current project."
  (let ((base-name (file-name-sans-extension (buffer-file-name)))
        (newfile ))
    (when (string= (file-name-extension base-name) "t")
      (setq base-name (file-name-sans-extension base-name)))
    (setq newfile (concat base-name ".h"))
    (if (file-readable-p newfile)
        newfile
      nil)
    ))



;;
;;  shu-cpp-find-h-definition
;;
(defun shu-cpp-find-h-definition ()
  "While in a cpp file, position point on a variable name that is defined in the
corresponding header file and invoke this function.  It will find all occurrences of
the name in the header file and put them in the message area."
  (interactive)
  (let ((hfile)
        (fbuf)
        (file-buf)
        (var-name)
        (lines)
        (spoint))
    (setq hfile (shu-simple-hother-file))
    (if (not hfile)
        (progn
          (ding)
          (message "%s" "There is no other h file")
          )
      (setq fbuf (get-file-buffer hfile))
      (if fbuf
          (setq file-buf fbuf)
        (setq file-buf (find-file-noselect hfile)))
      (setq var-name (shu-cpp-get-variable-name))
      (with-current-buffer file-buf
        (goto-char (point-min))
        (setq lines (shu-cpp-find-variable-name-lines-by-token var-name))
        (if (not lines)
            (progn
              (ding)
              (message "%s not found" var-name))
          (message "%s" lines)))
      (when (not fbuf) ;; We created buffer
        (kill-buffer file-buf)))
    ))



;;
;;  shu-cpp-get-variable-name
;;
(defun shu-cpp-get-variable-name ()
  "If point is sitting on something that looks like a legal variable name, return it,
otherwise, return nil."
  (let ((target-char shu-cpp-name)
        (target-name (concat shu-cpp-name "+"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (var-name))
    (save-excursion
      (when (looking-at target-char) ;; Looking at a legal variable name character
        (while (and (looking-at target-char) ;; Still on a variable name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a variable name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (when (re-search-forward target-name eol t)
          (setq var-name (match-string 0))
          ))) ;; Have something that matches variable name syntax
    var-name
    ))



;;
;;  shu-cpp-get-variable-name-position
;;
(defun shu-cpp-get-variable-name-position ()
  "If point is sitting on something that looks like a legal variable name,
return a cons cell that contains the start and end positions of the name
otherwise, return nil."
  (let ((target-char shu-cpp-name)
        (target-name (concat shu-cpp-name "+"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (start-pos)
        (end-pos)
        (ret-val))
    (save-excursion
      (when (looking-at target-char) ;; Looking at a legal variable name character
        (while (and (looking-at target-char) ;; Still on a variable name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a variable name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (when (re-search-forward target-name eol t)
          (setq start-pos (match-beginning 0))
          (setq end-pos (match-end 0))
          (setq ret-val (cons start-pos end-pos))
          ))) ;; Have something that matches variable name syntax
    ret-val
    ))



;;
;;  shu-cpp-find-variable-name-by-token
;;
(defun shu-cpp-find-variable-name-by-token (var-name)
  "Tokenize the entire buffer and return the position of the first token
that matches var-name."
  (let ((token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq token (shu-cpp-token-extract-token token-info))
        (when (string= token var-name)
          (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)))
      (setq tlist (cdr tlist)))
    spoint
    ))



;;
;;  shu-cpp-find-variable-name-lines-by-token
;;
(defun shu-cpp-find-variable-name-lines-by-token (var-name)
  "Tokenize the entire buffer and return a string that is composed of each
line that contains the token."
  (let ((token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (line)
        (got-lines)
        (lines "")
        (prefix "")
        (line-no))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq token (shu-cpp-token-extract-token token-info))
        (when (string= token var-name)
          (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
          (goto-char spoint)
          (setq line-no (number-to-string (shu-current-line)))
          (setq line
                (concat line-no ": "
                        (shu-trim
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
          (setq lines (concat lines prefix line))
          (setq prefix "\n")
          (setq got-lines t)))
      (setq tlist (cdr tlist)))
    (when (not got-lines)
      (setq lines nil))
    lines
    ))



;;
;;  shu-to-snake
;;
(defun shu-to-snake ()
  "Convert the variable name at point from camel case to snake case.

For example, \"mumbleSomethingOther\" becomes \"mumble_something_other\"."
  (interactive)
  (let ((pos (shu-cpp-get-variable-name-position))
        (start-pos)
        (end-pos)
        (case-fold-search nil))
    (if (not pos)
        (progn
          (ding)
          (message "%s" "Not sitting on a variable name"))
      (save-excursion
        (setq start-pos (car pos))
        (setq end-pos (cdr pos))
        (goto-char start-pos)
        (while (re-search-forward "\\([A-Z]\\)" end-pos t)
          (replace-match (concat "_" (downcase (match-string 1))) t t)
          (setq end-pos (1+ end-pos)))))
    ))




;;
;;  shu-to-camel
;;
(defun shu-to-camel ()
  "Convert the variable name at point from snake case to camel case.

For example, \"mumble_something_other\" becomes \"mumbleSomethingOther\"."
  (interactive)
  (let ((gb (get-buffer-create "**boo**"))
        (pos (shu-cpp-get-variable-name-position))
        (start-pos)
        (end-pos)
        (looking)
        (i)
        (c)
        (case-fold-search nil))
    (if (not pos)
        (progn
          (ding)
          (message "%s" "Not sitting on a variable name"))
      (save-excursion
        (setq start-pos (car pos))
        (setq end-pos (cdr pos))
        (goto-char start-pos)
        (while (< (point) end-pos)
          (setq c (buffer-substring-no-properties (point) (1+ (point))))
          (princ (concat "c: '" c "'\n") gb)
          (if (string= c "_")
              (progn
                (setq looking t)
                (delete-region (point) (1+ (point)))
                (setq end-pos (1- end-pos)))
            (when looking
              (setq c (upcase c))
              (delete-region (point) (1+ (point)))
              (insert c)
              (setq looking nil)))
          (when (not looking)
            (forward-char 1)))))
    ))



;;
;;  shu-cpp-make-date
;;
(defun shu-cpp-make-date ()
  "insert a string that is the list of values to be passed to the constructor of
 a date type that accepts year, month, day."
  (interactive)
  (insert (concat (shu-cpp-internal-make-date) ";"))
  )



;;
;;  shu-cpp-make-datetime
;;
(defun shu-cpp-make-datetime ()
  "insert a string that is the list of values to be passed to the constructor of
 a datetime type that accepts year, month, day, hour, minute, second,
 milliseconds, microseconds."
  (interactive)
  (insert (concat (shu-cpp-internal-make-datetime) ";"))
  )



;;
;;  shu-cpp-internal-make-bool
;;
(defun shu-cpp-internal-make-bool ()
  "Return  value for a bool type."
  "(false)"
  )



;;
;;  shu-cpp-internal-make-char
;;
(defun shu-cpp-internal-make-char ()
  "Return a string that can be used to initialize a test variable of type int."
  (interactive)
  (let ((min 0)
        (max 126))
    (concat "(" (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-internal-make-date
;;
(defun shu-cpp-internal-make-date ()
  "Return a string that is the list of values to be passed to the constructor of
 a datetime type that accepts year, month, day, hour, minute, second,
 milliseconds, microseconds."
  (let ((year (number-to-string (shu-random-range (- (shu-current-year) 8) (shu-current-year))))
        (month (number-to-string (shu-random-range 1 12)))
        (day (number-to-string (shu-random-range 1 27))))
    (concat "(" year ", " month ", " day ")")
    ))



;;
;;  shu-cpp-internal-make-datetime
;;
(defun shu-cpp-internal-make-datetime ()
  "Return a string that is the list of values to be passed to the constructor of
 a datetime type that accepts year, month, day, hour, minute, second,
 milliseconds, microseconds."
  (let ((year (number-to-string (shu-random-range (- (shu-current-year) 8) (shu-current-year))))
        (month (number-to-string (shu-random-range 1 12)))
        (day (number-to-string (shu-random-range 1 27)))
        (hour (number-to-string (shu-random-range 0 23)))
        (minute (number-to-string (shu-random-range 0 59)))
        (second (number-to-string (shu-random-range 0 59)))
        (milli (number-to-string (shu-random-range 0 999)))
        (micro (number-to-string (shu-random-range 0 999))))
    (concat "(" year ", " month ", " day ", " hour ", " minute ", " second ", " milli ", " micro ")")
    ))



;;
;;  shu-cpp-tz-make-datetime
;;
(defun shu-cpp-tz-make-datetime ()
  "insert a string that is the list of values to be passed to the constructor of
 a datetime type that accepts year, month, day, hour, minute, second,
 milliseconds, microseconds."
  (interactive)
  (insert (concat (shu-cpp-internal-tz-make-datetime) ";"))
  )



;;
;;  shu-cpp-internal-tz-make-datetime
;;
(defun shu-cpp-internal-tz-make-datetime ()
  "Return a string that is the list of values to be passed to the constructor of
a timezone datetime type."
  (concat "(" shu-cpp-datetime-type (shu-cpp-internal-make-datetime) ", 0)")
  )



;;
;;  shu-cpp-internal-make-double
;;
(defun shu-cpp-internal-make-double ()
  "Return a string that can be used to initialize a test variable of type double."
  (interactive)
  (let ((min 0)
        (max 12345678))
    (concat "(" (number-to-string (shu-random-range min max)) "."
            (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-internal-make-float
;;
(defun shu-cpp-internal-make-float ()
  "Return a string that can be used to initialize a test variable of type float."
  (interactive)
  (let ((min 0)
        (max 12345))
    (concat "(" (number-to-string (shu-random-range min max)) "."
            (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-internal-make-int
;;
(defun shu-cpp-internal-make-int ()
  "Return a string that can be used to initialize a test variable of type lint."
  (interactive)
  (let ((min 102)
        (max 2147483647))
    (concat "(" (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-make-interval
;;
(defun shu-cpp-make-interval ()
  "insert a string that is the list of values to be passed to the constructor of a datetime
 type that accepts year, month, day, hour, minute, second, milliseconds, microseconds."
  (interactive)
  (insert (concat (shu-cpp-internal-make-interval) ";"))
  )



;;
;;  shu-cpp-internal-make-interval
;;
(defun shu-cpp-internal-make-interval ()
  "Return a string that is the list of values to be passed to the constructor of a datetime
 type that accepts days, hours, minutes, seconds, milliseconds, microseconds."
  (let ((days "0")
        (hours (number-to-string (shu-random-range 0 23)))
        (minutes (number-to-string (shu-random-range 0 59)))
        (seconds (number-to-string (shu-random-range 0 59)))
        (millis (number-to-string (shu-random-range 0 999)))
        (micros (number-to-string (shu-random-range 0 999))))
    (concat "(" days ", " hours ", " minutes ", " seconds ", " millis ", " micros ")")
    ))



;;
;;  shu-cpp-make-short-interval
;;
(defun shu-cpp-make-short-interval ()
  "insert a string that is the list of values to be passed to the constructor of a datetime
 type that accepts year, month, day, hour, minute, second, milliseconds, microseconds."
  (interactive)
  (insert (concat (shu-cpp-internal-make-short-interval) ";"))
  )



;;
;;  shu-cpp-internal-make-short-interval
;;
(defun shu-cpp-internal-make-short-interval ()
  "Return a string that is the list of values to be passed to the constructor of a datetime
 type that accepts seconds and nanoseconds."
  (let ((days "0")
        (seconds (number-to-string (shu-random-range 0 999999)))
        (nanos (number-to-string (shu-random-range 0 999999))))
    (concat "(" seconds ", " nanos ")")
    ))



;;
;;  shu-cpp-internal-make-long-long
;;
(defun shu-cpp-internal-make-long-long ()
  "Return a string that can be used to initialize a test variable of type long long."
  (interactive)
  (let ((min 12345678901)
        (max 123456789012345678))
    (concat "(" (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-internal-make-short
;;
(defun shu-cpp-internal-make-short ()
  "Return a string that can be used to initialize a test variable of type short."
  (interactive)
  (let ((min 102)
        (max 32768))
    (concat "(" (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-make-size-type
;;
(defun shu-cpp-make-size-type ()
  "insert a string that is a possible value for a size type."
  (shu-cpp-internal-make-unsigned-int)
  )



;;
;;  shu-cpp-make-time
;;
(defun shu-cpp-make-time ()
  "insert a string that is the list of values to be passed to the constructor of
a time type that accepts hour, minute, second,  milliseconds, microseconds."
  (interactive)
  (insert (concat (shu-cpp-internal-make-time) ";"))
  )



;;
;;  shu-cpp-internal-make-time
;;
(defun shu-cpp-internal-make-time ()
  "Return a string that is the list of values to be passed to the constructor of
 a time type that accepts hour, minute, second,  milliseconds, microseconds."
  (let ((hour (number-to-string (shu-random-range 0 23)))
        (minute (number-to-string (shu-random-range 0 59)))
        (second (number-to-string (shu-random-range 0 59)))
        (milli (number-to-string (shu-random-range 0 999)))
        (micro (number-to-string (shu-random-range 0 999))))
    (concat "(" hour ", " minute ", " second ", " milli ", " micro ")")
    ))



;;
;;  shu-cpp-internal-make-unsigned-int
;;
(defun shu-cpp-internal-make-unsigned-int ()
  "Return a string that can be used to initialize a test variable of type lint."
  (interactive)
  (let ((min 102)
        (max 4294967295))
    (concat "(" (number-to-string (shu-random-range min max)) ")")
    ))



;;
;;  shu-cpp-fill-test-data
;;
(defun shu-cpp-fill-test-data ()
  "If the data type at the beginning of the line is a recognized data type, then
fill in a random value for that data type at point.  This allows someone writing
a test to declare a data type and a name and then call this function.  If the
author creates a line that looks like this and then invokes this function

     std::string   abc

The line will be transformed into one that looks something like this:

     std::string   abc(\"RDATZC\");

The recognized data types are the ones that are defined by the custom variables
shu-cpp-date-type, shu-cpp-datetime-timezone-type, shu-cpp-datetime-type,
shu-cpp-interval-type, shu-cpp-long-long-type, shu-cpp-string-type, or
shu-cpp-time-type plus many of the standard C++ types, such as int, bool, short,
etc.

The data types may optionally be preceded by \"const\".

If the last character of the line is \";\", it is deleted before a data type is
filled in with a new semi-colon following it."
  (interactive)
  (let ((did-fill (shu-cpp-internal-fill-test-data)))
    (when (not did-fill)
      (ding)
      (message "%s" "Unrecognized data type"))
    ))





;;
;;  shu-cpp-internal-fill-test-data
;;
(defun shu-cpp-internal-fill-test-data ()
  "If the data type at the beginning of the line is a recognized data type, then
fill in a random value for that data type at point.  This allows someone writing
a test to declare a data type and a name and then call this function.  If the
author creates a line that looks like this and then invokes this function

     std::string   abc

The line will be transformed into one that looks something like this:

     std::string   abc(\"RDATZC\");

The recognized data types are the ones that are defined by the custom variables
shu-cpp-date-type, shu-cpp-datetime-timezone-type, shu-cpp-datetime-type,
shu-cpp-interval-type, shu-cpp-long-long-type, shu-cpp-string-type, or
shu-cpp-time-type plus many of the standard C++ types, such as int, bool, short,
etc.

The data types may optionally be preceded by \"const\".

If the last character of the line is \";\", it is deleted before a data type is
filled in with a new semi-colon following it.

Return t if a recognized data type was found and a value was filled in."
  (let* ((gb (get-buffer-create "**foo**"))
         (bool-type "bool")
         (char-type "char")
         (double-type "double")
         (float-type "float")
         (int-type "int")
         (short-type "short")
         (xx)
         (data)
         (fail-type)
         (bol (line-beginning-position))
         (eol (line-end-position))
         (semi "")
         (ss
          (concat
           "\\s-*"
           "[const ]*"
           "\\s-*"
           "\\("
           "["
           shu-cpp-datetime-timezone-type "\\|"
           shu-cpp-interval-type "\\|"
           shu-cpp-datetime-type "\\|"
           shu-cpp-size-type "\\|"
           shu-cpp-string-type "\\|"
           shu-cpp-time-type "\\|"
           shu-cpp-long-long-type "\\|"
           char-type "\\|"
           bool-type "\\|"
           double-type "\\|"
           float-type "\\|"
           int-type "\\|"
           short-type
           "]+"
           "\\)"))
         (did-fill))
    (princ (concat ss "\n\n") gb)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward ss eol t)
        (setq xx (match-string 1))
        (princ (concat "xx: [" xx "]\n") gb)
        (cond
         ((string= xx shu-cpp-datetime-timezone-type)
          (setq data (shu-cpp-internal-tz-make-datetime)))
         ((string= xx shu-cpp-interval-type)
          (setq data (shu-cpp-internal-make-interval)))
         ((string= xx shu-cpp-short-interval-type)
          (setq data (shu-cpp-internal-make-short-interval)))
         ((string= xx shu-cpp-date-type)
          (setq data (shu-cpp-internal-make-date)))
         ((string= xx shu-cpp-datetime-type)
          (setq data (shu-cpp-internal-make-datetime)))
         ((string= xx shu-cpp-size-type)
          (setq data (shu-cpp-make-size-type)))
         ((string= xx shu-cpp-string-type)
          (setq data (concat "(\"" (shu-misc-random-ua-string 6) "\")")))
         ((string= xx shu-cpp-time-type)
          (setq data (shu-cpp-internal-make-time)))
         ((string= xx shu-cpp-long-long-type)
          (setq data (shu-cpp-internal-make-long-long)))
         ((string= xx bool-type)
          (setq data (shu-cpp-internal-make-bool)))
         ((string= xx "har")
          (setq data (shu-cpp-internal-make-char)))
         ((string= xx double-type)
          (setq data (shu-cpp-internal-make-double)))
         ((string= xx float-type)
          (setq data (shu-cpp-internal-make-float)))
         ((string= xx int-type)
          (setq data (shu-cpp-internal-make-int)))
         ((string= xx "hort")
          (setq data (shu-cpp-internal-make-short)))
         )))
    (if data
        (progn
          (end-of-line)
          (backward-char 1)
          (if (looking-at ";")
              (setq semi "")
            (setq semi ";")
            (end-of-line)
            )
          (insert (concat data semi))
          (setq did-fill t))
      (ding)
      (message "%s" "Unrecognized data type"))
    did-fill
    ))



;;
;;  shu-cpp-fill-test-area
;;
(defun shu-cpp-fill-test-area (start end)
  "For all lines between the marked start and end points, if a recognized data
type has been declared on a line, fill it with random test data.

For the benefit of unit tests, this function returns a a cons cell whose car is
the number of unrecognized data types and whose cdr is the number of values
generated."
  (interactive "r")
  (let ((sline (shu-the-line-at start))
        (eline (shu-the-line-at end))
        (line-diff 0)
        (eol)
        (did-fill)
        (fill-count 0)
        (skip-count 0)
        (count 0))
    (save-excursion
      (goto-char start)
      (while (and (<= (shu-current-line) eline)
                  (= line-diff 0)
                  )
        (end-of-line)
        (when (> (- (line-end-position) (line-beginning-position)) 5)
          (setq did-fill (shu-cpp-internal-fill-test-data))
          (if did-fill
              (setq fill-count (1+ fill-count))
            (setq skip-count (1+ skip-count))))
        (setq line-diff (forward-line 1))))
    (if (= skip-count 0)
        (message "%d values filled in" fill-count)
      (message "%d values filled in, %d data types unrecognized" fill-count skip-count))
    (cons skip-count fill-count)
    ))




;;
;;  shu-gcc
;;
(defun shu-gcc ()
  "Get compile command command from current buffer.  While in a compile buffer,
go to the top of the buffer, search for the end of the prompt line, collect the
rest of the line and put it into the kill ring.  This takes the string that was
used for the last compile command in the current buffer and puts it into the
kill ring.  To compile again with the same command, kill the buffer, open a new
shell, and yank."
  (interactive)
  (let ((eol)
        (cc))
    (save-excursion
      (goto-char (point-min))
      (setq eol (save-excursion (move-end-of-line nil) (point)))
      (if (search-forward "$" eol t)
          (progn
            (forward-char 1)
            (copy-region-as-kill (point) eol)
            (with-temp-buffer
              (yank)
              (setq cc (buffer-substring-no-properties (point-min) (point-max))))
            (message "%s" cc))
        (ding)
        (message "%s" "*** Not found ***")))
    ))



;;
;;  shu-cpp-general-set-alias
;;
(defun shu-cpp-general-set-alias ()
  "Set the common alias names for the functions in shu-cpp-general.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'cpp1-class 'shu-cpp1-class)
  (defalias 'cpp2-class 'shu-cpp2-class)
  (defalias 'new-c-class 'shu-new-c-class)
  (defalias 'operators 'shu-operators)
  (defalias 'shu-dox-hdr 'shu-shu-dox-hdr)
  (defalias 'dox-brief 'shu-dox-brief)
  (defalias 'dox2-hdr 'shu-dox2-hdr)
  (defalias 'dcc 'shu-dcc)
  (defalias 'dce 'shu-dce)
  (defalias 'clc 'shu-clc)
  (defalias 'drc 'shu-drc)
  (defalias 'binclude 'shu-binclude)
  (defalias 'ginclude 'shu-ginclude)
  (defalias 'dox-cvt 'shu-dox-cvt)
  (defalias 'dox-cbt 'shu-dox-cbt)
  (defalias 'new-x-file 'shu-new-x-file)
  (defalias 'new-h-file 'shu-new-h-file)
  (defalias 'new-c-file 'shu-new-c-file)
  (defalias 'author 'shu-author)
  (defalias 'getters 'shu-getters)
  (defalias 'get-set 'shu-get-set)
  (defalias 'csplit 'shu-csplit)
  (defalias 'cunsplit 'shu-cunsplit)
  (defalias 'creplace 'shu-creplace)
  (defalias 'set-modern 'shu-set-modern)
  (defalias 'set-no-modern 'shu-set-no-modern)
  (defalias 'cif 'shu-cif)
  (defalias 'celse 'shu-celse)
  (defalias 'cfor 'shu-cfor)
  (defalias 'cwhile 'shu-cwhile)
  (defalias 'cdo 'shu-cdo)
  (defalias `new-deallocate `shu-new-deallocate)
  (defalias `citerate `shu-citerate)
  (defalias `cciterate `shu-cciterate)
  (defalias 'diterate 'shu-diterate)
  (defalias 'dciterate 'shu-dciterate)
  (defalias 'titerate 'shu-titerate)
  (defalias 'tciterate 'shu-tciterate)
  (defalias 'ck 'shu-cpp-check-streaming-op)
  (defalias 'set-default-namespace 'shu-set-default-namespace)
  (defalias 'qualify-class 'shu-interactive-qualify-class-name)
  (defalias 'qualify-std 'shu-qualify-namespace-std)
  (defalias 'qualify-bsl 'shu-qualify-namespace-bsl)
  (defalias 'dbx-malloc 'shu-dbx-summarize-malloc)
  (defalias 'fixp 'shu-cpp-fix-prototype)
  (defalias 'getdef 'shu-cpp-find-h-definition)
  (defalias 'to-snake 'shu-to-snake)
  (defalias 'to-camel 'shu-to-camel)
  (defalias 'make-datetime 'shu-cpp-make-datetime)
  (defalias 'make-tzdate 'shu-cpp-tz-make-datetime)
  (defalias 'make-interval 'shu-cpp-make-interval)
  (defalias 'fill-data 'shu-cpp-fill-test-data)
  (defalias 'fill-area 'shu-cpp-fill-test-area)
  (defalias 'gcc 'shu-gcc)
  )

;;; shu-cpp-general.el ends here
