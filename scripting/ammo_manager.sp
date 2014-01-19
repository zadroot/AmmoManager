/**
* Ammo Manager by Root
*
* Description:
*   Allows to setup different clips and reserved ammo for any weapons as well as enabling realistic reload.
*
* Version 0.9
* Changelog & more info at http://goo.gl/4nKhJ
*/

// ====[ INCLUDES ]=================================================
#include <sdkhooks>
#include <sdktools>

// ====[ CONSTANTS ]================================================
#define PLUGIN_NAME    "Ammo Manager"
#define PLUGIN_VERSION "0.9"

enum weapontype
{
	save, // this option saves original clip size
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
	bool:IsCSGO, prefixlen, MAX_AMMOCVARS, MAX_WEAPONS; // Max. bounds for ammo convars and m_hMyWeapons array datamap

// ====[ PLUGIN ]===================================================
public Plugin:myinfo =
{
	name        = PLUGIN_NAME,
	author      = "Root",
	description = "Allows to setup different clips and reserved ammo for any weapons as well as enabling realistic reload",
	version     = PLUGIN_VERSION,
	url         = "http://dodsplugins.com/",
};


/* APLRes:AskPluginLoad2()
 *
 * Called before the plugin starts up.
 * ----------------------------------------------------------------- */
public APLRes:AskPluginLoad2(Handle:myself, bool:late, String:error[], err_max)
{
	// Mark GetEngineVersion as optional native due to older SM versions and GetEngineVersionCompat() stock
	MarkNativeAsOptional("GetEngineVersion");
	return APLRes_Success;
}

/* OnPluginStart()
 *
 * When the plugin starts up.
 * ------------------------------------------------------------------ */
public OnPluginStart()
{
	// Create version ConVar
	CreateConVar("sm_ammo_manager_version", PLUGIN_VERSION, PLUGIN_NAME, FCVAR_NOTIFY|FCVAR_DONTRECORD);

	// Register ConVars without using global handles
	decl Handle:registar; // Hook ConVar changes and set global booleans when convar is just created
	HookConVarChange((registar = CreateConVar("sm_ammo_enabled", "1", "Whether or not enable Ammo Manager plugin",   FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); enabled       = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_setclip", "0", "Whether or not set clip size (experimental)", FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); saveclips     = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_reserve", "1", "Whether or not set reserved ammo settings",   FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); reserveammo   = GetConVarBool(registar);
	HookConVarChange((registar = CreateConVar("sm_ammo_realism", "0", "Whether or not use realism reloading mode",   FCVAR_PLUGIN, true, 0.0, true, 1.0)), OnConVarChange); realismreload = GetConVarBool(registar);
	CloseHandle(registar);

	// Find and store offsets properly, because those will be used a bit often
	m_iAmmo              = FindSendPropOffsEx("CBasePlayer",       "m_iAmmo");
	m_hMyWeapons         = FindSendPropOffsEx("CBasePlayer",       "m_hMyWeapons");
	m_hOwner             = FindSendPropOffsEx("CBaseCombatWeapon", "m_hOwner");
	m_iClip1             = FindSendPropOffsEx("CBaseCombatWeapon", "m_iClip1");
	m_iClip2             = FindSendPropOffsEx("CBaseCombatWeapon", "m_iClip2");
	m_iPrimaryAmmoType   = FindSendPropOffsEx("CBaseCombatWeapon", "m_iPrimaryAmmoType");
	m_iSecondaryAmmoType = FindSendPropOffsEx("CBaseCombatWeapon", "m_iSecondaryAmmoType");

	// I assume other games (such as DoD:S) got 'weapon_' prefix and 48 max weapons
	prefixlen   = 7;
	MAX_WEAPONS = 48;

	// Thanks to Powerlord for this stock
	new EngineVersion:version = GetEngineVersionCompat();
	switch (version)
	{
		case Engine_CSS, Engine_CSGO:
		{
			// Set MAX_AMMOCVARS for CS:S to 10
			MAX_AMMOCVARS = 10;

			if (version == Engine_CSGO)
			{
				// Set global CS:GO boolean to properly check for weapons with silencer
				IsCSGO = true;

				// Setup appropriate values for CS:GO as well
				MAX_AMMOCVARS = 12;
				MAX_WEAPONS   = 64;
			}

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

			// Find property offset to check whether or not weapon got given silencer
			m_bSilencerOn = FindSendPropOffsEx("CWeaponCSBaseGun", "m_bSilencerOn");
		}
		case Engine_TF2: prefixlen = 10; // Because TF2 got 'tf_weapon_' prefix
	}

	// I love tries
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
		// First cvar is about to toggle plugin
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
	// If plugin is enabled and its should set reserved ammo, reset values
	if (enabled && reserveammo)
		SetConVarBool(convar, false);
}

/* OnMapStart()
 *
 * When the map starts.
 * ------------------------------------------------------------------ */
public OnMapStart()
{
	// Get the config and prepare clipnammo value to set trie array
	decl String:filepath[PLATFORM_MAX_PATH], Handle:file, clipnammo[array_size];
	BuildPath(Path_SM, filepath, sizeof(filepath), "configs/ammo_manager.txt");

	// Check whether or not plugin config is exists
	if ((file = OpenFile(filepath, "r")) != INVALID_HANDLE)
	{
		ClearTrie(WeaponsTrie);

		decl String:fileline[PLATFORM_MAX_PATH];
		decl String:datas[3][PLATFORM_MAX_PATH];

		// Read every line in config
		while (ReadFileLine(file, fileline, PLATFORM_MAX_PATH))
		{
			// Break ; symbols from config (a javalia's method)
			if (ExplodeString(fileline, ";", datas, sizeof(datas), PLATFORM_MAX_PATH) == 3)
			{
				// Properly setup default clip size and other ammo values
				clipnammo[defaultclip] = 0; // Set clip to 0 so no garbage is written to array
				clipnammo[clipsize] = StringToInt(datas[1]);
				clipnammo[ammosize] = StringToInt(datas[2]);
				SetTrieArray(WeaponsTrie, datas[0], clipnammo, array_size);
			}
		}
	}
	else
	{
		// No config, wtf? Restore ammo settings and properly disable plugin then
		RestoreAmmoSetup(false, true);
		SetFailState("Unable to load plugin configuration file \"%s\"!", file);
	}

	// KyleS hates handles
	CloseHandle(file);
}

/* OnEntityCreated()
 *
 * When an entity is created.
 * ------------------------------------------------------------------ */
public OnEntityCreated(entity, const String:classname[])
{
	decl dummy[1];

	// Hook entity spawning if plugin is enabling and entity classname is exists in trie array
	if (enabled && GetTrieArray(WeaponsTrie, classname[prefixlen], dummy, 0))
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

		// Owner is not used at all in initialize weapon type, but why not just pass it?
		SetWeaponReservedAmmo(GetEntDataEnt2(weapon, m_hOwner), weapon, weapontype:init);

		if (saveclips)
		{
			// Setup default weapon clips after a small delay, because spawn hook is too fast for that
			CreateTimer(0.1, Timer_SetupDefaultClips, EntIndexToEntRef(weapon), TIMER_FLAG_NO_MAPCHANGE);
		}
	}
}

/* OnClientPutInServer()
 *
 * Called when a client is entering the game.
 * ------------------------------------------------------------------ */
public OnClientPutInServer(client)
{
	// Use both spawn hooks as a backend if equipment didnt changed
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
	// And set after player spawns
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
	// Set saved weapon clip and its ammo for this client on weapon pickup
	SetWeaponClip(weapon, weapontype:pickup);
	SetWeaponReservedAmmo(client, weapon, weapontype:pickup);
}

/* OnWeaponReloaded()
 *
 * Called when a weapon is about to reload.
 * ------------------------------------------------------------------ */
public Action:OnWeaponReload(weapon)
{
	decl String:classname[64], clipnammo[array_size];
	GetEdictClassname(weapon,  classname, sizeof(classname));

	// Check whether or not this weapon got silencer initialized
	if (IsCSGO && GetEntData(weapon, m_bSilencerOn))
		StrCat(classname, sizeof(classname), "-s");

	// Make sure this weapon is exists in Weapons Trie
	if (GetTrieArray(WeaponsTrie, classname[prefixlen], clipnammo, array_size))
	{
		// If clip size is more than 0 and less than defined, deny weapon reloading
		if (0 < clipnammo[clipsize] <= GetEntData(weapon, m_iClip1))
		{
			return Plugin_Handled;
		}
		else
		{
			// Otherwise create timer to correct clipsize and ammo during reloading
			new Handle:data = INVALID_HANDLE, client = GetEntDataEnt2(weapon, m_hOwner);
			CreateDataTimer(0.1, Timer_FixAmmunition, data, TIMER_REPEAT|TIMER_FLAG_NO_MAPCHANGE);

			// Add entity reference, client userid and weapon clips in data timer
			WritePackCell(data, EntIndexToEntRef(weapon));
			WritePackCell(data, GetClientUserId(client));
			WritePackCell(data, clipnammo[defaultclip]);
			WritePackCell(data, clipnammo[clipsize]);
		}

		// If realistic reload is set and player wants to reload garand, set garand clip size to 0 (DoD:S perks)
		if (realismreload && StrEqual(classname[7], "garand"))
			SetEntData(weapon, m_iClip1, 0);
	}

	return Plugin_Continue;
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
	new client  = GetClientOfUserId(ReadPackCell(data));
	new oldclip = ReadPackCell(data);
	new newclip = ReadPackCell(data);

	// If weapon reference or client is invalid, stop timer immediately
	if (weapon == INVALID_ENT_REFERENCE || !client)
	{
		return Plugin_Stop;
	}

	// To find WeaponID in m_iAmmo array we should add multiplied m_iPrimaryAmmoType datamap offset by 4 onto m_iAmmo player array (meh)
	new WeaponID = GetEntData(weapon, m_iPrimaryAmmoType);

	// Get the current player ammo for this weapon
	new currammo = GetEntData(client, m_iAmmo + (WeaponID * 4));

	// Timer is created twice for some reason when reload hook is fired, so this is a small fix
	static bool:reloaded[MAXPLAYERS + 1] = true;

	// Does weapon is reloading at the moment?
	if (GetEntProp(weapon, Prop_Data, "m_bInReload"))
	{
		// Check whether or not player got any ammo and havent reloaded before
		if (reloaded[client] && currammo)
		{
			if (realismreload)
			{
				// Reset clip to 0 for realistic reload
				SetEntData(weapon, m_iClip1, 0);
			}
			if (saveclips)
			{
				// Correct new clip
				if (newclip > oldclip)
					newclip = oldclip
				else if (newclip < oldclip) // Correct ammo once during player reload
					SetEntData(client, m_iAmmo + (WeaponID * 4), currammo + newclip);
			}
		}

		// Set boolean to make sure that clip has been set and ammo been corrected once
		reloaded[client] = false;
	}
	else // Nope player is not reloading anymore
	{
		// Plugin should save different clips?
		if (saveclips && !reloaded[client])
		{
			// Yea, setup desired weapon clip after reloading
			SetEntData(weapon, m_iClip1, newclip);
		}

		// Player has reloaded, stop timer
		reloaded[client] = true;
		return Plugin_Stop;
	}

	return Plugin_Continue;
}

/* Timer_SetupDefaultClips()
 *
 * Sets default weapon clips after they spawn.
 * ------------------------------------------------------------------ */
public Action:Timer_SetupDefaultClips(Handle:timer, any:ref)
{
	// Convert entity reference to entity index
	new weapon = EntRefToEntIndex(ref)
	if (weapon != INVALID_ENT_REFERENCE)
	{
		// And now set default weapon clips in weapons trie
		SetWeaponClip(weapon, weapontype:save);
	}
}

/* SetSpawnAmmunition()
 *
 * Sets player reserved ammo properly in pre and post spawn hooks.
 * ------------------------------------------------------------------ */
SetSpawnAmmunition(client, bool:prehook)
{
	// Loop through max game weapons to properly check player weapons in m_hMyWeapons array
	for (new i; i < MAX_WEAPONS; i++)
	{
		new weapon = GetEntDataEnt2(client, m_hMyWeapons + (i * 4));

		// Validate!
		if (IsValidEdict(weapon))
		{
			// On pre-spawn hook set m_iSecondaryAmmoType as an initialized value
			SetWeaponClip(weapon, prehook ? init : pickup); // Otherwise set ammo as current
			SetWeaponReservedAmmo(client, weapon, prehook ? init : pickup);
		}
	}
}

/* SetWeaponClip()
 *
 * Sets weapon clip when its spawned, picked or dropped.
 * ------------------------------------------------------------------ */
SetWeaponClip(weapon, type)
{
	// Does plugin should save weapon clips?
	if (saveclips && IsValidEdict(weapon))
	{
		// Retrieve weapon classname
		decl String:classname[64], clipnammo[array_size];
		GetEdictClassname(weapon,  classname, sizeof(classname));

		// If weapon got initialized silencer, add '-s' end
		if (IsCSGO && GetEntData(weapon, m_bSilencerOn))
			StrCat(classname, sizeof(classname), "-s");

		// Ignore first 10 characters in TF2 because weapon classnames got tf_weapon_ prefix
		if (GetTrieArray(WeaponsTrie, classname[prefixlen], clipnammo, array_size))
		{
			switch (type)
			{
				case save:
				{
					// Default clip size from array cant be less than original
					if (clipnammo[defaultclip] < GetEntData(weapon, m_iClip1))
					{
						// Correct the clip and set it in trie array
						clipnammo[defaultclip] = GetEntData(weapon, m_iClip1);
						SetTrieArray(WeaponsTrie, classname[prefixlen], clipnammo, sizeof(clipnammo));
					}
				}
				case init:
				{
					// Otherwise store desired clip value in unused m_iClip2 datamap offset
					SetEntData(weapon, m_iClip2, clipnammo[clipsize]);
				}
				case drop:
				{
					// When weapon is dropped, get proper clip value and set m_iClip2 yet again
					SetEntData(weapon, m_iClip2, GetEntData(weapon, m_iClip1) > clipnammo[clipsize] ? clipnammo[clipsize] : GetEntData(weapon, m_iClip1));
				}
				case pickup:
				{
					// Make sure desired clip size isnt bigger than default one, if so, correct it and set m_iClip2 as default clip value
					SetEntData(weapon, m_iClip1, GetEntData(weapon, m_iClip2) > clipnammo[clipsize] ? clipnammo[clipsize] : GetEntData(weapon, m_iClip2));
				}
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
	// Make sure weapon is valid
	if (reserveammo && IsValidEdict(weapon))
	{
		decl String:classname[64], clipnammo[array_size];
		GetEdictClassname(weapon,  classname, sizeof(classname));

		// '-s' needed to detect m4a1 and usp-s in CS:GO
		if (IsCSGO && GetEntData(weapon, m_bSilencerOn))
			StrCat(classname, sizeof(classname), "-s");

		if (GetTrieArray(WeaponsTrie, classname[prefixlen], clipnammo, array_size))
		{
			// Get the weapon ID to properly find it in m_iAmmo array
			new WeaponID = GetEntData(weapon, m_iPrimaryAmmoType);

			// If max. ammo value is not set, dont do anything
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