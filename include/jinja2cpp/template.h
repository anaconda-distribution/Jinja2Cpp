#ifndef JINJA2_TEMPLATE_H
#define JINJA2_TEMPLATE_H

#include "config.h"
#include "error_info.h"
#include "value.h"

#include <nonstd/expected.hpp>

#include <iostream>
#include <memory>
#include <string>

namespace jinja2
{
class JINJA2CPP_EXPORT ITemplateImpl;
class JINJA2CPP_EXPORT TemplateEnv;
template<typename CharT>
class TemplateImpl;
template<typename U>
using Result = nonstd::expected<U, ErrorInfo>;
template<typename U>
using ResultW = nonstd::expected<U, ErrorInfoW>;

template<typename CharT>
struct MetadataInfo
{
    std::string metadataType;
    nonstd::basic_string_view<CharT> metadata;
    SourceLocation location;
};

/*!
 * \brief Template object which is used to render narrow char templates
 *
 * This class is a main class for rendering narrow char templates. It can be used independently or together with
 * \ref TemplateEnv. In the second case it's possible to use templates inheritance and extension.
 *
 * Basic usage of Template class:
 * ```c++
 * std::string source = "Hello World from Parser!";
 *
 * jinja2::Template tpl;
 * tpl.Load(source);
 * std::string result = tpl.RenderAsString(ValuesMap{}).value();
 * ```
 */
class JINJA2CPP_EXPORT Template
{
public:
    /*!
     * \brief Default constructor
     */
    Template()
        : Template(nullptr)
    {
    }
    /*!
     * \brief Initializing constructor
     *
     * Creates instance of the template with the specified template environment object
     *
     * @param env Template environment object which created template should refer to
     */
    explicit Template(TemplateEnv* env);
    /*!
     * Destructor
     */
    ~Template();

    /*!
     * \brief Load template from the zero-terminated narrow char string
     *
     * Takes specified narrow char string and parses it as a Jinja2 template. In case of error returns detailed
     * diagnostic
     *
     * @param tpl      Zero-terminated narrow char string with template description
     * @param tplName  Optional name of the template (for the error reporting purposes)
     *
     * @return Either noting or instance of \ref ErrorInfoTpl as an error
     */
    Result<void> Load(const char* tpl, std::string tplName = std::string());
    /*!
     * \brief Load template from the std::string
     *
     * Takes specified std::string object and parses it as a Jinja2 template. In case of error returns detailed
     * diagnostic
     *
     * @param str      std::string object with template description
     * @param tplName  Optional name of the template (for the error reporting purposes)
     *
     * @return Either noting or instance of \ref ErrorInfoTpl as an error
     */
    Result<void> Load(const std::string& str, std::string tplName = std::string());
    /*!
     * \brief Load template from the stream
     *
     * Takes specified stream object and parses it as a source of Jinja2 template. In case of error returns detailed
     * diagnostic
     *
     * @param stream   Stream object with template description
     * @param tplName  Optional name of the template (for the error reporting purposes)
     *
     * @return Either noting or instance of \ref ErrorInfoTpl as an error
     */
    Result<void> Load(std::istream& stream, std::string tplName = std::string());
    /*!
     * \brief Load template from the specified file
     *
     * Loads file with the specified name and parses it as a source of Jinja2 template. In case of error returns
     * detailed diagnostic
     *
     * @param fileName Name of the file to load
     *
     * @return Either noting or instance of \ref ErrorInfoTpl as an error
     */
    Result<void> LoadFromFile(const std::string& fileName);

    /*!
     * \brief Render previously loaded template to the narrow char stream
     *
     * Renders previously loaded template to the specified narrow char stream and specified set of params.
     *
     * @param os      Stream to render template to
     * @param params  Set of params which should be passed to the template engine and can be used within the template
     *
     * @return Either noting or instance of \ref ErrorInfoTpl as an error
     */
    Result<void> Render(std::ostream& os, const ValuesMap& params);
    /*!
     * \brief Render previously loaded template to the narrow char string
     *
     * Renders previously loaded template as a narrow char string and with specified set of params.
     *
     * @param params  Set of params which should be passed to the template engine and can be used within the template
     *
     * @return Either rendered string or instance of \ref ErrorInfoTpl as an error
     */
    Result<std::string> RenderAsString(const ValuesMap& params);
    /*!
     * \brief Get metadata, provided in the {% meta %} tag
     *
     * @return Parsed metadata as a generic map value or instance of \ref ErrorInfoTpl as an error
     */
    Result<GenericMap> GetMetadata();
    /*!
     * \brief Get non-parsed metadata, provided in the {% meta %} tag
     *
     * @return Non-parsed metadata information or instance of \ref ErrorInfoTpl as an error
     */
    Result<MetadataInfo<char>> GetMetadataRaw();

private:
    std::shared_ptr<ITemplateImpl> m_impl;
    friend class TemplateImpl<char>;
};
} // jinja2

#endif // JINJA2_TEMPLATE_H
