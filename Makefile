PROJECT = nifoc_ct_helper

ERLC_OPTS = -Werror +debug_info +warn_bif_clash +warn_deprecated_function +warn_deprecated_type \
				+warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_unused_import \
				+warn_unused_function +warn_unused_record +warn_unused_vars +warnings_as_errors

include erlang.mk
