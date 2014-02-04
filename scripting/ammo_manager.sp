/**
* Ammo Manager by Root
*
* Description:
*   Allows to setup different clips and reserved ammo for any weapons as well as enabling realistic reload.
*
* Version 1.0
* Changelog & more info at http://goo.gl/4nKhJ
*/

// ====[ INCLUDES ]=================================================
#include <sdkhooks>
#include <sdktools>
#undef REQUIRE_EXTENSIONS
#include <cstrike>

// ====[ CONSTANTS ]================================================
#define PLUGIN_NAME    "Ammo Manager"
#define PLUGIN_VERSION "1.0"

enum weapontype
{
	init,
	drop,
	pickup
};

enum ammotype // values in trie array
{
	defaultclip, // original clip size
	clipsize,    // desired clip size
	ammosize,    // reserved ammo
	array_size   // size of trie array
};

// Ammo setup convars for CS:S and CS:GO
static const String:ammocvars[][] =
{
	"ammo_338mag_max",
	"ammo_357sig_max",
	"ammo_45acp_max",
	"ammo_50AE_max",
	"ammo_556mm_box_max",
	"ammo_556mm_max",
	"ammo_57mm_max",
	"ammo_762mm_max",
	"ammo_9mm_max",
	"ammo_buckshot_max",
	"ammo_357sig_small_max", // Ammo for usp-s
	"ammo_556mm_small_max" // And for m4a1-s (CS:GO)
};

// ====[ VARIABLES ]================================================
new	Handle:WeaponsTrie, // Trie array to save weapon names, its clips and reserved ammo
	ammosetup[sizeof(ammocvars)], // Array to store original ammo convar values
	bool:enabled, bool:saveclips, // Global booleans to use instead of global handles
	bool:reserveammo, bool:realismreload,
	m_iAmmo, m_hMyWeapons, m_hOwner, // Datamap offsets to setup ammunition
	m_iClip1, m_iClip2, m_bSilencerOn,
	m_iPrimaryAmmoType, m_iSecondaryAmmoType,
	prefixlength, MAX_AMMOCVARS, MAX_WEAPONS; // Max. bounds for ammo convars and m_hMyWeapons array datamap

// ====[ PLUGIN ]===================================================
public Plugin:myinfo =
{
	name        = PLUGIN_NAME,
	author      = "Root",
	description = "Allows to setup different clips and reserved ammo for any weapons as well as enabling realistic reload",
	version     = PLUGIN_VERSION,
	url         = "http://dodsplugins.com/",
}


/* APLRes:AskPluginLoad2()
 *
 * Called before the plugin starts up.
 * ----------------------------------------------------------------- */
public APLRes:AskPluginLoad2(Handle:myself, bool:late, String:error[], err_max)
{
	// Mark GetEngineVersion as optional native due to older SM versions and GetEngineVersionCompat() stock
	MarkNativeAsOptional("GetEngineVersion");
}

/* OnPluginStart()
 *
 * When the plugin starts up.
 * ------------------------------------------------------------------ */
public OnPluginStart()
{
	// Find and store offsets properly, because those will be used a bit often
	m_iAmmo              = FindSendPropOffsEx("CBasePlayer",       "m_iAmmo");
	m_hMyWeapons         = FindSendPropOffsEx("CBasePlayer",       "m_hMyWeapons");
	m_hOwner             = FindSendPropOffsEx("CBaseCombatWeapon", "m_hOwner");
	m_iClip1             = FindSendPropOffsEx("CBaseCombatWeapon", "m_iClip1");
	m_iClip2             = FindSendPropOffsEx("CBaseCombatWeapon", "m_iClip2");
	m_iPrimaryAmmoType   = FindSendPropOffsEx("CBaseCombatWeapon", "m_iPrimaryAmmoType");
	m_iSecondaryAmmoType = FindSendPropOffsEx("CBaseCombatWeapon", "m_iSecondaryAmmoType");

	// Create version ConVar
	CreateConVar("sm_ammo_manager_version", PLUGIN_VERSION, PLUGIN_NAME, FCVAR_NOTIFY|FCVAR_DONTRECORD);

	// Register ConVars without using global handles
	decl Handle:registar; // Hook ConVar changes and set global booleans when convar is just created
	HookConVarChange((registar = CreateConVar("sm_ammo_enabled", "1", "Whether or not enable Ammo Manager plugin",   FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); enabled       = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_setclip", "0", "Whether or not set clip size (experimental)", FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); saveclips     = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_reserve", "1", "Whether or not set reserved ammo settings",   FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); reserveammo   = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_realism", "0", "Whether or not enable realistic reload mode", FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); realismreload = GetConVarBool(registar);
	CloseHandle(registar);

	// I assume other games (such as DoD:S) got 'weapon_' prefix and 48 weapons
	prefixlength = 7;
	MAX_WEAPONS  = 48;

	new EngineVersion:version = GetEngineVersionCompat();
	switch (version)
	{
		case Engine_CSS, Engine_CSGO:
		{
			if (version == Engine_CSGO)
			{
				// Setup appropriate values for CS:GO
				MAX_AMMOCVARS = 12;
				MAX_WEAPONS   = 64;

				// Find property offset to check whether or not weapon got the silencer
				m_bSilencerOn = FindSendPropOffsEx("CWeaponCSBaseGun", "m_bSilencerOn");
			}
			else MAX_AMMOCVARS = 10; // Set MAX_AMMOCVARS for CS:S to 10

			// Loop though all cvars
			for (new i; i < MAX_AMMOCVARS; i++)
			{
				// Cache default convar value
				ammosetup[i] = GetConVarInt(FindConVar(ammocvars[i]));

				// Set all ammo values to 0. Its required to properly set reserved ammo settings for weapons (i.e. dont override)
				SetConVarBool(FindConVar(ammocvars[i]), false);

				// Hook ammo convars changes
				HookConVarChange(FindConVar(ammocvars[i]), OnAmmoSettingsChanged);
			}
		}
		case Engine_TF2: prefixlength = 10; // Because TF2 got 'tf_weapon_' prefix, which is 10 chars long
	}

	WeaponsTrie = CreateTrie();
	AutoExecConfig();
}

/* OnConVarChange()
 *
 * Called when ConVar value has changed.
 * ------------------------------------------------------------------ */
public OnConVarChange(Handle:convar, const String:oldValue[], const String:newValue[])
{
	// Retrieve the convar name
	decl String:cvarName[16];
	GetConVarName(convar, cvarName, sizeof(cvarName));

	// Now compare chars from convar names to properly set global booleans
	switch (cvarName[11])
	{
		case 'b': RestoreAmmoSetup(bool:StringToInt(newValue), true); // second arg means plugin has toggled
		case 'c': saveclips      = bool:StringToInt(newValue);
		case 'e': RestoreAmmoSetup(bool:StringToInt(newValue), false); // only reserved ammo has been changed
		case 'l': realismreload  = bool:StringToInt(newValue);
	}
}

/* OnAmmoSettingsChanged()
 *
 * Called when ammo cvars settings has changed.
 * ------------------------------------------------------------------ */
public OnAmmoSettingsChanged(Handle:convar, const String:oldValue[], const String:newValue[])
{
	// If plugin is enabled and should set reserved ammo, reset changes
	if (enabled && reserveammo)
		SetConVarBool(convar, false);
}

/* OnMapStart()
 *
 * When the map starts.
 * ------------------------------------------------------------------ */
public OnMapStart()
{
	// Get the config and set weapons trie values eventually
	decl String:filepath[PLATFORM_MAX_PATH], Handle:file, clipnammo[array_size];
	BuildPath(Path_SM, filepath, sizeof(filepath), "configs/ammo_manager.txt");

	// Check whether or not plugin config is exists
	if ((file = OpenFile(filepath, "r")) != INVALID_HANDLE)
	{
		ClearTrie(WeaponsTrie);

		decl String:fileline[PLATFORM_MAX_PATH];
		decl String:datas[4][PLATFORM_MAX_PATH];

		// Read every line in config
		while (ReadFileLine(file, fileline, sizeof(fileline)))
		{
			// Break ; symbols from config (a javalia's method)
			if (ExplodeString(fileline, ";", datas, sizeof(datas), sizeof(datas[])) == 4)
			{
				// Properly setup default clip size and other ammo values
				clipnammo[defaultclip] = StringToInt(datas[1]);
				clipnammo[clipsize]    = StringToInt(datas[2]);
				clipnammo[ammosize]    = StringToInt(datas[3]);
				SetTrieArray(WeaponsTrie, datas[0], clipnammo, array_size);
			}
		}
	}
	else
	{
		// No config, wtf? Restore ammo settings and properly disable plugin
		RestoreAmmoSetup(false, true);
		SetFailState("Unable to load plugin configuration file \"%s\"!", filepath);
	}

	// Close config handle
	CloseHandle(file);
}

/* OnEntityCreated()
 *
 * When an entity is created.
 * ------------------------------------------------------------------ */
public OnEntityCreated(entity, const String:classname[])
{
	decl dummy[1];

	// Hook entity spawning if plugin is enabling and entity classname is exists in a trie
	if (enabled && GetTrieArray(WeaponsTrie, classname[prefixlength], dummy, 0))
	{
		SDKHook(entity, SDKHook_SpawnPost, OnWeaponSpawned);
	}
}

/* OnWeaponSpawned()
 *
 * When a weapon is successfully spawned.
 * ------------------------------------------------------------------ */
public OnWeaponSpawned(weapon)
{
	// Use SDKHookEx in case if not weapon was passed (due to ignored first 7/10 characters)
	if (SDKHookEx(weapon, SDKHook_Reload, OnWeaponReload))
	{
		// Set weapon clip and reserve ammo when its just spawned
		SetWeaponClip(weapon, weapontype:init);

		// Just pass weapon owner, even if he is not yet exists
		SetWeaponReservedAmmo(GetEntDataEnt2(weapon, m_hOwner), weapon, weapontype:init);
	}
}

/* OnClientPutInServer()
 *
 * Called when a client is entering the game.
 * ------------------------------------------------------------------ */
public OnClientPutInServer(client)
{
	// Use both spawn hooks as a backend if equipment did not changed
	SDKHook(client, SDKHook_Spawn,           OnPlayerSpawn);
	SDKHook(client, SDKHook_SpawnPost,       OnPlayerSpawnPost);
	SDKHook(client, SDKHook_WeaponDropPost,  OnWeaponDropPost);
	SDKHook(client, SDKHook_WeaponEquipPost, OnWeaponEquipPost);
}

/* OnPlayerSpawn()
 *
 * Called when the player spawns.
 * ------------------------------------------------------------------ */
public Action:OnPlayerSpawn(client)
{
	// Set ammunition before player equips a weapon
	SetSpawnAmmunition(client, true);
}

/* OnPlayerSpawnPost()
 *
 * Called after the player spawns.
 * ------------------------------------------------------------------ */
public OnPlayerSpawnPost(client)
{
	// And correct ammo after player spawns
	SetSpawnAmmunition(client, false);
}

/* OnWeaponDropPost()
 *
 * Called when a client drops his weapon.
 * ------------------------------------------------------------------ */
public OnWeaponDropPost(client, weapon)
{
	// Save weapon clip and its ammo after dropping
	SetWeaponClip(weapon, weapontype:drop);
	SetWeaponReservedAmmo(client, weapon, weapontype:drop);
}

/* OnWeaponEquipPost()
 *
 * Called when a client equips a weapon.
 * ------------------------------------------------------------------ */
public OnWeaponEquipPost(client, weapon)
{
	// Set stored weapon clip and its ammo for this client on every weapon pickup
	SetWeaponClip(weapon, weapontype:pickup);
	SetWeaponReservedAmmo(client, weapon, weapontype:pickup);
}

/* OnWeaponReload()
 *
 * Called when a weapon is about to reload.
 * ------------------------------------------------------------------ */
public Action:OnWeaponReload(weapon)
{
	decl String:classname[64], clipnammo[array_size];
	GetEdictClassname(weapon,  classname, sizeof(classname));

	// Does silencer offset found and weapon got this initialized?
	if (bool:m_bSilencerOn && GetEntData(weapon, m_bSilencerOn))
		StrCat(classname, sizeof(classname), "-s");

	// Make sure this weapon is exists in Weapons Trie
	if (GetTrieArray(WeaponsTrie, classname[prefixlength], clipnammo, array_size))
	{
		// If clip size is more than 0 and less than defined, deny weapon reloading
		if (saveclips && 0 < clipnammo[clipsize] <= GetEntData(weapon, m_iClip1))
		{
			return Plugin_Handled;
		}
		else
		{
			// Otherwise create timer to correct clipsize/ammo during reloading
			new Handle:data = INVALID_HANDLE, client = GetEntDataEnt2(weapon, m_hOwner);
			CreateDataTimer(0.1, Timer_FixAmmunition, data, TIMER_REPEAT|TIMER_FLAG_NO_MAPCHANGE);

			// Add entity reference, client serial and weapon clips in data timer
			WritePackCell(data, EntIndexToEntRef(weapon));
			WritePackCell(data, GetClientSerial(client));
			WritePackCell(data, clipnammo[defaultclip]);
			WritePackCell(data, clipnammo[clipsize]);
		}

		// Reset garand clipsize to 0 if realistic reload enabled (a DoD:S ReloadGarand-like perks)
		if (realismreload && StrEqual(classname[7], "garand"))
			SetEntData(weapon, m_iClip1, 0);
	}

	return Plugin_Continue;
}

/* CS_OnBuyCommand()
 *
 * Called when a player attempts to purchase an item.
 * ------------------------------------------------------------------ */
public Action:CS_OnBuyCommand(client, const String:weapon[])
{
	// No need to create timer unless plugin should save weapon clips
	if (saveclips)
	{
		// Create a delay when player buys a weapon
		CreateTimer(0.1, Timer_PostBuyEquip, GetClientSerial(client), TIMER_FLAG_NO_MAPCHANGE);
	}
}

/* Timer_FixAmmunition()
 *
 * Called during weapon is reloading.
 * ------------------------------------------------------------------ */
public Action:Timer_FixAmmunition(Handle:event, any:data)
{
	if (data == INVALID_HANDLE)
	{
		LogError("Invalid data timer!");
		return Plugin_Stop;
	}

	ResetPack(data);

	// Retrieve all the data from timer
	new weapon  = EntRefToEntIndex(ReadPackCell(data));
	new client  = GetClientFromSerial(ReadPackCell(data));
	new oldclip = ReadPackCell(data);
	new newclip = ReadPackCell(data);

	// If weapon reference or client is invalid, stop timer immediately
	if (weapon == INVALID_ENT_REFERENCE || !client)
	{
		return Plugin_Stop;
	}

	// To find WeaponID in m_iAmmo array we should add multiplied m_iPrimaryAmmoType datamap offset by 4 onto m_iAmmo player array, meh
	new WeaponID = GetEntData(weapon, m_iPrimaryAmmoType);

	// And get the current player ammo for this weapon
	new currammo = GetEntData(client, m_iAmmo + (WeaponID * 4));
	new realclip = GetEntData(weapon, m_iClip1), fixedammo;

	// Timer is created twice for some reason when reload hook is fired, so this is a fix
	static bool:reloaded[MAXPLAYERS + 1] = true;

	// Does weapon is reloading at the moment?
	if (bool:GetEntProp(weapon, Prop_Data, "m_bInReload", true))
	{
		// Check if player got any ammo and haven't reloaded before
		if (reloaded[client] && currammo)
		{
			if (realismreload)
			{
				// Reset clip to 0 for realistic reload
				SetEntData(weapon, m_iClip1, 0);
			}
			if (saveclips && newclip != oldclip)
			{
				// Correct player ammo once during reloading
				if (newclip > oldclip)
					fixedammo = currammo - (newclip - realclip);
				else if (newclip < oldclip)
					fixedammo = currammo + (oldclip - newclip);

				// Shh about issue
				SetEntData(client, m_iAmmo + (WeaponID * 4), fixedammo);
			}
		}

		// Set boolean to make sure that clip has been set and ammo corrected once
		reloaded[client] = false;
	}
	else // Player is not reloading anymore
	{
		// Plugin should save different clips?
		if (saveclips && !reloaded[client])
		{
			if (currammo + realclip >= newclip)
				SetEntData(weapon, m_iClip1, newclip);
		}

		// Player has reloaded, stop timer
		reloaded[client] = true;
		return Plugin_Stop;
	}

	return Plugin_Continue;
}

/* Timer_PostBuyCommand()
 *
 * Sets default weapon clips after they spawn.
 * ------------------------------------------------------------------ */
public Action:Timer_PostBuyEquip(Handle:timer, any:client)
{
	// Validate client
	if ((client = GetClientFromSerial(client)))
		SetSpawnAmmunition(client, false);
}

/* SetSpawnAmmunition()
 *
 * Sets player reserved ammo properly in pre and post spawn hooks.
 * ------------------------------------------------------------------ */
SetSpawnAmmunition(client, bool:prehook)
{
	// Yet again check if enabled
	if (enabled)
	{
		// Loop through max game weapons to properly get all player weapons
		for (new i; i < MAX_WEAPONS; i++)
		{
			new weapon  = -1;
			if ((weapon = GetEntDataEnt2(client, m_hMyWeapons + (i * 4))) != -1)
			{
				// On pre-spawn hook set m_iSecondaryAmmoType as default value
				SetWeaponClip(weapon, prehook ? init : pickup); // Otherwise set current ammo
				SetWeaponReservedAmmo(client, weapon, prehook ? init : pickup);
			}
		}
	}
}

/* SetWeaponClip()
 *
 * Sets weapon clip when its spawned, picked or dropped.
 * ------------------------------------------------------------------ */
SetWeaponClip(weapon, type)
{
	// Check if plugin is enabled
	if (enabled && saveclips && IsValidEdict(weapon))
	{
		// Retrieve weapon classname
		decl String:classname[64], clipnammo[array_size];
		GetEdictClassname(weapon,  classname, sizeof(classname));

		// If weapon got silencer since spawn, add '-s' to end
		if (bool:m_bSilencerOn && GetEntData(weapon, m_bSilencerOn))
			StrCat(classname, sizeof(classname), "-s");

		// Ignore first 7/10 characters in weapon string to avoid comparing with the prefix
		if (GetTrieArray(WeaponsTrie, classname[prefixlength], clipnammo, array_size))
		{
			switch (type)
			{
				case init:   SetEntData(weapon, m_iClip2, clipnammo[clipsize]); // When weapon just spawned, set m_iClip2 value to clip size from trie array
				case drop:   SetEntData(weapon, m_iClip2, GetEntData(weapon, m_iClip1)); // After dropping a weapon, set m_iClip2 value same as current (m_iClip1) size
				case pickup: SetEntData(weapon, m_iClip1, GetEntData(weapon, m_iClip2)); // And when weapon is picked, retrieve m_iClip2 value and set current clip size
			}
		}
	}
}

/* SetWeaponReservedAmmo()
 *
 * Sets weapon reserved ammunition when its spawned, picked or dropped.
 * ------------------------------------------------------------------ */
SetWeaponReservedAmmo(client, weapon, type)
{
	// Validate weapon
	if (reserveammo && IsValidEdict(weapon))
	{
		decl String:classname[64], clipnammo[array_size];
		GetEdictClassname(weapon,  classname, sizeof(classname));

		// Add '-s' to detect m4a1 and usp-s in CS:GO
		if (bool:m_bSilencerOn && GetEntData(weapon, m_bSilencerOn))
			StrCat(classname, sizeof(classname), "-s");

		if (GetTrieArray(WeaponsTrie, classname[prefixlength], clipnammo, array_size))
		{
			// Get the weapon ID to properly find it in m_iAmmo array
			new WeaponID = GetEntData(weapon, m_iPrimaryAmmoType);

			// If ammo value is not set ( = 0), dont do anything
			if (clipnammo[ammosize])
			{
				// Retrieve ammunition type (when its created, dropped or picked)
				switch (type)
				{
					case init:   SetEntData(weapon, m_iSecondaryAmmoType, clipnammo[ammosize]); // Initialize reserved ammunition in unused m_iSecondaryAmmoType datamap offset
					case drop:   if (IsClientInGame(client)) SetEntData(weapon, m_iSecondaryAmmoType, GetEntData(client, m_iAmmo + (WeaponID * 4)));
					case pickup: if (IsClientInGame(client)) SetEntData(client, m_iAmmo + (WeaponID * 4), GetEntData(weapon, m_iSecondaryAmmoType)); // Retrieve it
				}
			}
		}
	}
}

/* RestoreAmmoSetup()
 *
 * Restores original ammo settings for CS:S and CS:GO when plugin disables.
 * ------------------------------------------------------------------ */
RestoreAmmoSetup(bool:value, bool:toggled)
{
	if (toggled) enabled = value; // Plugin has toggled? set enabled value then
	else     reserveammo = value; // Otherwise seems like reserved ammo has changed

	new EngineVersion:version = GetEngineVersionCompat();
	switch (version)
	{
		case Engine_CSS, Engine_CSGO:
		{
			// Again loop through all game cvars
			for (new i; i < MAX_AMMOCVARS; i++)
			{
				// Set ammo cvar values to 0 if plugin is enabled, otherwise set original ammo values for all cstrike cvars
				SetConVarInt(FindConVar(ammocvars[i]), (toggled && enabled || !toggled && reserveammo) ? 0 : ammosetup[i]);
			}
		}
	}
}

/* FindSendPropOffsEx()
 *
 * Returns the offset of the specified network property.
 * ------------------------------------------------------------------ */
FindSendPropOffsEx(const String:serverClass[64], const String:propName[64])
{
	new offset = FindSendPropOffs(serverClass, propName);

	// Disable plugin if a networkable send property offset was not found
	if (offset <= 0)
	{
		SetFailState("Unable to find offset \"%s::%s\"!", serverClass, propName);
	}

	return offset;
}

stock EngineVersion:GetEngineVersionCompat()
{
	new EngineVersion:version;
	if (GetFeatureStatus(FeatureType_Native, "GetEngineVersion") != FeatureStatus_Available)
	{
		new sdkVersion = GuessSDKVersion();
		switch (sdkVersion)
		{
			case SOURCE_SDK_ORIGINAL:
			{
				version = Engine_Original;
			}

			case SOURCE_SDK_DARKMESSIAH:
			{
				version = Engine_DarkMessiah;
			}

			case SOURCE_SDK_EPISODE1:
			{
				version = Engine_SourceSDK2006;
			}

			case SOURCE_SDK_EPISODE2:
			{
				version = Engine_SourceSDK2007;
			}

			case SOURCE_SDK_BLOODYGOODTIME:
			{
				version = Engine_BloodyGoodTime;
			}

			case SOURCE_SDK_EYE:
			{
				version = Engine_EYE;
			}

			case SOURCE_SDK_CSS:
			{
				version = Engine_CSS;
			}

			case SOURCE_SDK_EPISODE2VALVE:
			{
				decl String:gameFolder[PLATFORM_MAX_PATH];
				GetGameFolderName(gameFolder, PLATFORM_MAX_PATH);
				if (StrEqual(gameFolder, "dod", false))
				{
					version = Engine_DODS;
				}
				else if (StrEqual(gameFolder, "hl2mp", false))
				{
					version = Engine_HL2DM;
				}
				else
				{
					version = Engine_TF2;
				}
			}

			case SOURCE_SDK_LEFT4DEAD:
			{
				version = Engine_Left4Dead;
			}

			case SOURCE_SDK_LEFT4DEAD2:
			{
				decl String:gameFolder[PLATFORM_MAX_PATH];
				GetGameFolderName(gameFolder, PLATFORM_MAX_PATH);
				if (StrEqual(gameFolder, "nucleardawn", false))
				{
					version = Engine_NuclearDawn;
				}
				else
				{
					version = Engine_Left4Dead2;
				}
			}

			case SOURCE_SDK_ALIENSWARM:
			{
				version = Engine_AlienSwarm;
			}

			case SOURCE_SDK_CSGO:
			{
				version = Engine_CSGO;
			}

			default:
			{
				version = Engine_Unknown;
			}
		}
	}
	else
	{
		version = GetEngineVersion();
	}

	return version;
}