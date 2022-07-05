#ifndef JINJA2CPP_TEMPLATE_ENV_H
#define JINJA2CPP_TEMPLATE_ENV_H

#include "config.h"
#include "error_info.h"
#include "filesystem_handler.h"
#include "template.h"

#include <shared_mutex>
#include <unordered_map>

namespace jinja2
{

class IErrorHandler;
class IFilesystemHandler;

//! Compatibility mode for jinja2c++ engine
enum class Jinja2CompatMode
{
    None,          //!< Default mode
    Vesrsion_2_10, //!< Compatibility with Jinja2 v.2.10 specification
};

//! Global template environment settings
struct Settings
{
    /// Extensions set which should be supported
    struct Extensions
    {
        bool Do = false;  //!< Enable use of `do` statement
    };

    //! Enables use of line statements (yet not supported)
    bool useLineStatements = false;
    //! Enables blocks trimming the same way as it does python Jinja2 engine
    bool trimBlocks = false;
    //! Enables blocks stripping (from the left) the same way as it does python Jinja2 engine
    bool lstripBlocks = false;
    //! Templates cache size
    int cacheSize = 400;
    //! If auto_reload is set to true (default) every time a template is requested the loader checks if the source changed and if yes, it will reload the template
    bool autoReload = true;
    //! Extensions set enabled for templates
    Extensions extensions;
    //! Controls Jinja2 compatibility mode
    Jinja2CompatMode jinja2CompatMode = Jinja2CompatMode::None;
    //! Default format for metadata block in the templates
    std::string m_defaultMetadataType = "json";
};

class JINJA2CPP_EXPORT TemplateEnv
{
public:
    using TimePoint = std::chrono::system_clock::time_point;
    using TimeStamp = std::chrono::steady_clock::time_point;

    /*!
     * \brief Returns global settings for the environment
     *
     * @return Constant reference to the global settings
     */
    const Settings& GetSettings() const {return m_settings;}
    /*!
     * \brief Returns global settings for the environment available for modification
     *
     * @return Reference to the global settings
     */
    Settings& GetSettings() {return m_settings;}

    /*!
     * \brief Replace global settings for the environment with the new ones
     *
     * @param setts New settings
     */
    void SetSettings(const Settings& setts) {m_settings = setts;}

    void AddFilesystemHandler(std::string prefix, FilesystemHandlerPtr h)
    {
        m_filesystemHandlers.push_back(FsHandler{std::move(prefix), std::move(h)});
    }
    void AddFilesystemHandler(std::string prefix, IFilesystemHandler& h)
    {
        m_filesystemHandlers.push_back(FsHandler{std::move(prefix), std::shared_ptr<IFilesystemHandler>(&h, [](auto*) {})});
    }
    nonstd::expected<Template, ErrorInfo> LoadTemplate(std::string fileName);

    void AddGlobal(std::string name, Value val)
    {
        std::unique_lock<std::shared_timed_mutex> l(m_guard);
        m_globalValues[std::move(name)] = std::move(val);
    }
    void RemoveGlobal(const std::string& name)
    {
        std::unique_lock<std::shared_timed_mutex> l(m_guard);
        m_globalValues.erase(name);
    }

    template<typename Fn>
    void ApplyGlobals(Fn&& fn)
    {
        std::shared_lock<std::shared_timed_mutex> l(m_guard);
        fn(m_globalValues);
    }

private:
    template<typename CharT, typename T, typename Cache>
    auto LoadTemplateImpl(TemplateEnv* env, std::string fileName, const T& filesystemHandlers, Cache& cache);


private:
    struct FsHandler
    {
        std::string prefix;
        FilesystemHandlerPtr handler;
    };

    struct BaseTemplateInfo
    {
        nonstd::optional<TimePoint> lastModification;
        TimeStamp lastAccessTime;
        FilesystemHandlerPtr handler;
    };

    struct TemplateCacheEntry : public BaseTemplateInfo
    {
        Template tpl;
    };

    std::vector<FsHandler> m_filesystemHandlers;
    Settings m_settings;
    ValuesMap m_globalValues;
    std::shared_timed_mutex m_guard;
    std::unordered_map<std::string, TemplateCacheEntry> m_templateCache;
};

} // jinja2

#endif // JINJA2CPP_TEMPLATE_ENV_H
