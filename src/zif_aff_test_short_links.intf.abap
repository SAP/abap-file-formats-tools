INTERFACE zif_aff_test_short_links PUBLIC.
  "! $values {@link .data:co_mode}
  "! $default {@link .data:co_mode.standard}
  TYPES ty_mode TYPE c LENGTH 1.
  CONSTANTS:
    BEGIN OF co_mode,
      standard TYPE ty_mode VALUE 'S',
      "! $enumValue 'cloudDevelopment'
      cloud TYPE ty_mode VALUE 'C',
    END OF co_mode.

  "! $values {@link .data:co_execution}
  TYPES ty_execution TYPE c LENGTH 1.
  CONSTANTS:
    BEGIN OF co_execution,
      "! $availableWhen {@link .ty_settings.mode} = {@link .data:co_mode.standard}
      classic TYPE ty_execution VALUE 'C',
      modern TYPE ty_execution VALUE 'M',
    END OF co_execution.

  TYPES:
    BEGIN OF ty_settings,
      enabled TYPE abap_bool,
      mode TYPE ty_mode,
      "! $availableWhen {@link .ty_settings.enabled} = true
      "! $requiredWhen {@link .ty_settings.enabled} = true
      detail TYPE string,
      "! $availableWhen {@link .ty_settings.mode} = {@link .data:co_mode.cloud}
      cloud_detail TYPE string,
      execution TYPE ty_execution,
    END OF ty_settings.
ENDINTERFACE.
