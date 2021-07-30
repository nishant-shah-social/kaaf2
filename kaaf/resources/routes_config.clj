;; :client and :entity values would remain the same for all environments
{:client {:sdk "api/lib/2"
          :xhr "xhr"
          :anon ""}
 :entity {:sdk {:config "/config/"
                :faqs "/faqs/"
                :faq "/faqs/%s/"
                :faq-helpful "/faqs/%s/helpful/"
                :faq-unhelpful "/faqs/%s/unhelpful/"
                :issue "/issues/"
                :my-issues "/my-issues/"
                :message "/issues/%s/messages/"
                :messages "/issues/%s/messages/"
                :profiles "/profiles/"
                :section "/sections/%s/"
                :customer-survey "/issues/%s/customer-survey/"}
          :xhr {:message "/%s/message/"
                :issue "/%s/issue/"
                :tags "/%s/tags/"
                :cif "/cif/%s/"
                :issue-details "/%s/issue-details/%s/"}
          :anon {:login "/login/"}}}
