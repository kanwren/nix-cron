{ lib }:

let
  checkType = type: value:
    lib.asserts.assertMsg (type.check value) "expected ${type.description}";

  object = types: lib.types.addCheck lib.types.attrs (a:
    builtins.all (k: builtins.hasAttr k a && types.${k}.check a.${k})
    (builtins.attrNames types)
    &&
    builtins.removeAttrs a (builtins.attrNames types) == {}
  ) // {
    description = "attribute set of {${lib.concatMapStringsSep " " (k: "${k} = ${types.${k}.description};") (builtins.attrNames types)}}";
  };

  partialObject = types: lib.types.addCheck lib.types.attrs (a:
    builtins.all (k: builtins.hasAttr k a -> types.${k}.check a.${k})
    (builtins.attrNames types)
    &&
    builtins.removeAttrs a (builtins.attrNames types) == {}
  ) // {
    description = "attribute set of {${lib.concatMapStringsSep " " (k: "${k} = ${types.${k}.description};") (builtins.attrNames types)}}";
    types = types;
  };

  escapeNixString = str: lib.strings.escape ["$"] (builtins.toJSON str);

  litString = val: lib.types.addCheck lib.types.anything (x: x == val) // {
    description = escapeNixString val;
  };

  timeRange_t = object {
    type = litString "timeRange";
    start = lib.types.ints.unsigned;
    end = lib.types.ints.unsigned;
  };

  timeList_t = object {
    type = litString "timeList";
    values = lib.types.listOf (lib.types.either lib.types.ints.unsigned timeRange_t);
  };

  timeStep_t = object {
    type = litString "timeStep";
    increment = lib.types.ints.positive;
  };

  timeAt_t = object {
    type = litString "timeAt";
    value = lib.types.ints.unsigned;
  };

  timePart_t = lib.types.nullOr (lib.types.oneOf [
    # "0", "1", etc.
    timeAt_t
    # "0-1", etc.
    timeRange_t
    # "0,1,2", "0-8,9-12", etc.
    timeList_t
    # "*/5"
    timeStep_t
  ]);

  specialTime_t = object {
    type = litString "specialTime";
    value = litString "@reboot";
  };

  timeSpecifier_t = partialObject {
    minute = timePart_t;
    hour = timePart_t;
    dayOfMonth = timePart_t;
    month = timePart_t;
    dayOfWeek = timePart_t;
  };

  time_t = lib.types.either timeSpecifier_t specialTime_t;

  checkPartialObjectType = type: value:
    let
      inherit (type) types;
      extraAttrs = builtins.removeAttrs value (builtins.attrNames types);
    in
      lib.asserts.assertMsg (builtins.isAttrs value) "expected an attr set"
      &&
      builtins.all (k: builtins.hasAttr k value ->
        lib.asserts.assertMsg (types.${k}.check value.${k}) "expected ${k} to be ${types.${k}.description}")
      (builtins.attrNames types)
      &&
      lib.asserts.assertMsg (extraAttrs == {}) "unexpected keys: ${toString (builtins.attrNames extraAttrs)}";

  checkTimeType = v:
    if !specialTime_t.check v
    then checkPartialObjectType timeSpecifier_t v
    else checkType specialTime_t v;

  renderTimePart = part:
    assert (checkType timePart_t part);
    if timeAt_t.check part then
      toString part.value
    else if timeRange_t.check part then
      "${toString part.start}-${toString part.end}"
    else if timeList_t.check part then
      lib.strings.concatMapStringsSep " " renderTimePart part.values
    else if part == null then
      "*"
    else if timeStep_t.check part then
      "*/${toString part.increment}"
    else builtins.throw "unrecognized time part type";

  renderSpecialTime = t: t.value;

  renderTime = time:
    assert (checkTimeType time);
    if specialTime_t.check time then
      renderSpecialTime time
    else lib.strings.concatMapStringsSep " " renderTimePart [
      (time.minute or null)
      (time.hour or null)
      (time.dayOfMonth or null)
      (time.month or null)
      (time.dayOfWeek or null)
    ];
in

rec {
  at = num:
    assert (checkType lib.types.ints.unsigned num);
    { type = "timeAt"; value = num; };

  every = inc:
    assert (checkType lib.types.ints.positive inc);
    { type = "timeStep"; increment = inc; };

  range = start: end:
    assert (checkType lib.types.ints.unsigned start);
    assert (checkType lib.types.ints.unsigned end);
    assert start <= end;
    { type = "timeRange"; inherit start end; };

  list = elems:
    let
      types = lib.types.either lib.types.ints.unsigned timeRange_t;
    in assert (builtins.all (checkType types) elems);
      { type = "timeList"; values = elems; };

  systemJob = time: user: command:
    assert (checkTimeType time);
    assert (checkType lib.types.str user);
    let
      stripEnd = str:
        let removeGroup = group: if builtins.isList group then "" else group;
        in lib.concatMapStrings removeGroup (builtins.split "\n+$" str);
      noNewlines = str: !lib.strings.hasInfix "\n" str;
      strWithNoNewlines = lib.types.addCheck lib.types.str noNewlines // {
        description = "string with no newlines";
      };
      strippedCommand = stripEnd command;
    in assert (checkType strWithNoNewlines strippedCommand);
    "${renderTime time} ${user} ${strippedCommand}";

  userJob = time: command:
    assert (checkTimeType time);
    let
      stripEnd = str:
        let removeGroup = group: if builtins.isList group then "" else group;
        in lib.concatMapStrings removeGroup (builtins.split "\n+$" str);
      noNewlines = str: !lib.strings.hasInfix "\n" str;
      strWithNoNewlines = lib.types.addCheck lib.types.str noNewlines // {
        description = "string with no newlines";
      };
      strippedCommand = stripEnd command;
    in assert (checkType strWithNoNewlines strippedCommand);
    "${renderTime time} ${strippedCommand}";

  reboot = { type = "specialTime"; value = "@reboot"; };

  yearly = {
    minute = 0;
    hour = 0;
    dayOfMonth = 1;
    month = 1;
  };

  annually = yearly;

  monthly = {
    minute = 0;
    hour = 0;
    dayOfMonth = 1;
  };

  weekly = {
    minute = 0;
    hour = 0;
    dayOfWeek = 0;
  };

  midnight = {
    minute = 0;
    hour = 0;
  };

  daily = midnight;

  hourly = {
    minute = 0;
  };

  month = {
    january = 1;
    february = 2;
    march = 3;
    april = 4;
    may = 5;
    june = 6;
    july = 7;
    august = 8;
    september = 9;
    october = 10;
    november = 11;
    december = 12;
  };

  day = {
    sunday = 0;
    monday = 1;
    tuesday = 2;
    wedneday = 3;
    thursday = 4;
    friday = 5;
    saturday = 6;
  };
}
